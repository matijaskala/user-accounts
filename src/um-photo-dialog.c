/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*-
 *
 * Copyright 2009-2010  Red Hat, Inc,
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Written by: Matthias Clasen <mclasen@redhat.com>
 */

#include "config.h"

#include <stdlib.h>
#include <math.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#define GDK_PIXBUF_ENABLE_BACKEND
#include <gtk/gtk.h>
#include <act/act.h>
#include <gconf/gconf-client.h>

#ifdef HAVE_CHEESE
#include <cheese-avatar-chooser.h>
#include <cheese-camera-device.h>
#include <cheese-camera-device-monitor.h>
#endif /* HAVE_CHEESE */

#include "um-photo-dialog.h"
#include "cc-crop-area.h"
#include "um-utils.h"

#define ROW_SPAN 6

struct _UmPhotoDialog {
        GtkWidget *photo_popup;
        GtkWidget *popup_button;
        GtkWidget *crop_area;

#ifdef HAVE_CHEESE
        CheeseCameraDeviceMonitor *monitor;
        GtkWidget *take_photo_menuitem;
        guint num_cameras;
#endif /* HAVE_CHEESE */

        ActUser *user;
};

static void
crop_dialog_response (GtkWidget     *dialog,
                      gint           response_id,
                      UmPhotoDialog *um)
{
        GdkPixbuf *pb, *pb2;

        if (response_id != GTK_RESPONSE_ACCEPT) {
                um->crop_area = NULL;
                gtk_widget_destroy (dialog);
                return;
        }

        pb = cc_crop_area_get_picture (CC_CROP_AREA (um->crop_area));
        pb2 = gdk_pixbuf_scale_simple (pb, 96, 96, GDK_INTERP_BILINEAR);

        set_user_icon_data (um->user, pb2);

        g_object_unref (pb2);
        g_object_unref (pb);

        um->crop_area = NULL;
        gtk_widget_destroy (dialog);
}

static void
um_photo_dialog_crop (UmPhotoDialog *um,
                      GdkPixbuf     *pixbuf)
{
        GtkWidget *dialog;

        dialog = gtk_dialog_new_with_buttons ("",
                                              GTK_WINDOW (gtk_widget_get_toplevel (um->popup_button)),
                                              0,
                                              _("_Cancel"),
                                              GTK_RESPONSE_CANCEL,
                                              _("Select"),
                                              GTK_RESPONSE_ACCEPT,
                                              NULL);
        gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

        gtk_window_set_icon_name (GTK_WINDOW (dialog), "system-users");

        g_signal_connect (G_OBJECT (dialog), "response",
                          G_CALLBACK (crop_dialog_response), um);

        /* Content */
        um->crop_area           = cc_crop_area_new ();
        cc_crop_area_set_min_size (CC_CROP_AREA (um->crop_area), 48, 48);
        cc_crop_area_set_constrain_aspect (CC_CROP_AREA (um->crop_area), TRUE);
        cc_crop_area_set_picture (CC_CROP_AREA (um->crop_area), pixbuf);
        gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
                            um->crop_area,
                            TRUE, TRUE, 8);

        gtk_window_set_default_size (GTK_WINDOW (dialog), 400, 300);

        gtk_widget_show_all (dialog);
}

static void
file_chooser_response (GtkDialog     *chooser,
                       gint           response,
                       UmPhotoDialog *um)
{
        gchar *filename;
        GError *error;
        GdkPixbuf *pixbuf;

        if (response != GTK_RESPONSE_ACCEPT) {
                gtk_widget_destroy (GTK_WIDGET (chooser));
                return;
        }

        filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));

        error = NULL;
        pixbuf = gdk_pixbuf_new_from_file (filename, &error);
        if (pixbuf == NULL) {
                g_warning ("Failed to load %s: %s", filename, error->message);
                g_error_free (error);
        }
        g_free (filename);

        gtk_widget_destroy (GTK_WIDGET (chooser));

        um_photo_dialog_crop (um, pixbuf);
        g_object_unref (pixbuf);
}

static char *
expand_thumbnailing_script (const char *script,
			    const int   size, 
			    const char *inuri,
			    const char *outfile)
{
  GString *str;
  const char *p, *last;
  char *localfile, *quoted;
  gboolean got_in;

  str = g_string_new (NULL);
  
  got_in = FALSE;
  last = script;
  while ((p = strchr (last, '%')) != NULL)
    {
      g_string_append_len (str, last, p - last);
      p++;

      switch (*p) {
      case 'u':
	quoted = g_shell_quote (inuri);
	g_string_append (str, quoted);
	g_free (quoted);
	got_in = TRUE;
	p++;
	break;
      case 'i':
	localfile = g_filename_from_uri (inuri, NULL, NULL);
	if (localfile)
	  {
	    quoted = g_shell_quote (localfile);
	    g_string_append (str, quoted);
	    got_in = TRUE;
	    g_free (quoted);
	    g_free (localfile);
	  }
	p++;
	break;
      case 'o':
	quoted = g_shell_quote (outfile);
	g_string_append (str, quoted);
	g_free (quoted);
	p++;
	break;
      case 's':
	g_string_append_printf (str, "%d", size);
	p++;
	break;
      case '%':
	g_string_append_c (str, '%');
	p++;
	break;
      case 0:
      default:
	break;
      }
      last = p;
    }
  g_string_append (str, last);

  if (got_in)
    return g_string_free (str, FALSE);

  g_string_free (str, TRUE);
  return NULL;
}

typedef struct {
    gint width;
    gint height;
    gint input_width;
    gint input_height;
    gboolean preserve_aspect_ratio;
} SizePrepareContext;

static GdkPixbuf *
gnome_desktop_thumbnail_scale_down_pixbuf (GdkPixbuf *pixbuf,
					   int dest_width,
					   int dest_height)
{
	int source_width, source_height;
	int s_x1, s_y1, s_x2, s_y2;
	int s_xfrac, s_yfrac;
	int dx, dx_frac, dy, dy_frac;
	div_t ddx, ddy;
	int x, y;
	int r, g, b, a;
	int n_pixels;
	gboolean has_alpha;
	guchar *dest, *src, *xsrc, *src_pixels;
	GdkPixbuf *dest_pixbuf;
	int pixel_stride;
	int source_rowstride, dest_rowstride;

	if (dest_width == 0 || dest_height == 0) {
		return NULL;
	}
	
	source_width = gdk_pixbuf_get_width (pixbuf);
	source_height = gdk_pixbuf_get_height (pixbuf);

	g_assert (source_width >= dest_width);
	g_assert (source_height >= dest_height);

	ddx = div (source_width, dest_width);
	dx = ddx.quot;
	dx_frac = ddx.rem;
	
	ddy = div (source_height, dest_height);
	dy = ddy.quot;
	dy_frac = ddy.rem;

	has_alpha = gdk_pixbuf_get_has_alpha (pixbuf);
	source_rowstride = gdk_pixbuf_get_rowstride (pixbuf);
	src_pixels = gdk_pixbuf_get_pixels (pixbuf);

	dest_pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, has_alpha, 8,
				      dest_width, dest_height);
	dest = gdk_pixbuf_get_pixels (dest_pixbuf);
	dest_rowstride = gdk_pixbuf_get_rowstride (dest_pixbuf);

	pixel_stride = (has_alpha)?4:3;
	
	s_y1 = 0;
	s_yfrac = -dest_height/2;
	while (s_y1 < source_height) {
		s_y2 = s_y1 + dy;
		s_yfrac += dy_frac;
		if (s_yfrac > 0) {
			s_y2++;
			s_yfrac -= dest_height;
		}

		s_x1 = 0;
		s_xfrac = -dest_width/2;
		while (s_x1 < source_width) {
			s_x2 = s_x1 + dx;
			s_xfrac += dx_frac;
			if (s_xfrac > 0) {
				s_x2++;
				s_xfrac -= dest_width;
			}

			/* Average block of [x1,x2[ x [y1,y2[ and store in dest */
			r = g = b = a = 0;
			n_pixels = 0;

			src = src_pixels + s_y1 * source_rowstride + s_x1 * pixel_stride;
			for (y = s_y1; y < s_y2; y++) {
				xsrc = src;
				if (has_alpha) {
					for (x = 0; x < s_x2-s_x1; x++) {
						n_pixels++;
						
						r += xsrc[3] * xsrc[0];
						g += xsrc[3] * xsrc[1];
						b += xsrc[3] * xsrc[2];
						a += xsrc[3];
						xsrc += 4;
					}
				} else {
					for (x = 0; x < s_x2-s_x1; x++) {
						n_pixels++;
						r += *xsrc++;
						g += *xsrc++;
						b += *xsrc++;
					}
				}
				src += source_rowstride;
			}
			
			if (has_alpha) {
				if (a != 0) {
					*dest++ = r / a;
					*dest++ = g / a;
					*dest++ = b / a;
					*dest++ = a / n_pixels;
				} else {
					*dest++ = 0;
					*dest++ = 0;
					*dest++ = 0;
					*dest++ = 0;
				}
			} else {
				*dest++ = r / n_pixels;
				*dest++ = g / n_pixels;
				*dest++ = b / n_pixels;
			}
			
			s_x1 = s_x2;
		}
		s_y1 = s_y2;
		dest += dest_rowstride - dest_width * pixel_stride;
	}
	
	return dest_pixbuf;
}

static void
size_prepared_cb (GdkPixbufLoader *loader, 
		  int              width,
		  int              height,
		  gpointer         data)
{
  SizePrepareContext *info = data;
  
  g_return_if_fail (width > 0 && height > 0);
  
  info->input_width = width;
  info->input_height = height;
  
  if (width < info->width && height < info->height) return;
  
  if (info->preserve_aspect_ratio && 
      (info->width > 0 || info->height > 0)) {
    if (info->width < 0)
      {
        width = width * (double)info->height/(double)height;
        height = info->height;
      }
    else if (info->height < 0)
      {
        height = height * (double)info->width/(double)width;
        width = info->width;
      }
    else if ((double)height * (double)info->width >
             (double)width * (double)info->height) {
      width = 0.5 + (double)width * (double)info->height / (double)height;
      height = info->height;
    } else {
      height = 0.5 + (double)height * (double)info->width / (double)width;
      width = info->width;
    }
  } else {
    if (info->width > 0)
      width = info->width;
    if (info->height > 0)
      height = info->height;
  }
  
  gdk_pixbuf_loader_set_size (loader, width, height);
}

static GdkPixbuf *
_gdk_pixbuf_new_from_uri_at_scale (const char *uri,
                                   gint        width,
                                   gint        height,
                                   gboolean    preserve_aspect_ratio)
{
    gboolean result;
    char buffer[0x1000];
    gsize bytes_read;
    GdkPixbufLoader *loader;
    GdkPixbuf *pixbuf;	
    GdkPixbufAnimation *animation;
    GdkPixbufAnimationIter *iter;
    gboolean has_frame;
    SizePrepareContext info;
    GFile *file;
    GFileInfo *file_info;
    GInputStream *input_stream;

    g_return_val_if_fail (uri != NULL, NULL);

    input_stream = NULL;

    file = g_file_new_for_uri (uri);

    /* First see if we can get an input stream via preview::icon  */
    file_info = g_file_query_info (file,
                                   G_FILE_ATTRIBUTE_PREVIEW_ICON,
                                   G_FILE_QUERY_INFO_NONE,
                                   NULL,  /* GCancellable */
                                   NULL); /* return location for GError */
    if (file_info != NULL) {
        GObject *object;

        object = g_file_info_get_attribute_object (file_info,
                                                   G_FILE_ATTRIBUTE_PREVIEW_ICON);
        if (object != NULL && G_IS_LOADABLE_ICON (object)) {
            input_stream = g_loadable_icon_load (G_LOADABLE_ICON (object),
                                                 0,     /* size */
                                                 NULL,  /* return location for type */
                                                 NULL,  /* GCancellable */
                                                 NULL); /* return location for GError */
        }
        g_object_unref (file_info);
    }

    if (input_stream == NULL) {
        input_stream = G_INPUT_STREAM (g_file_read (file, NULL, NULL));
        if (input_stream == NULL) {
	    g_object_unref (file);
            return NULL;
        }
    }

    loader = gdk_pixbuf_loader_new ();
    if (1 <= width || 1 <= height) {
        info.width = width;
        info.height = height;
	info.input_width = info.input_height = 0;
        info.preserve_aspect_ratio = preserve_aspect_ratio;        
        g_signal_connect (loader, "size-prepared", G_CALLBACK (size_prepared_cb), &info);
    }

    has_frame = FALSE;

    result = FALSE;
    while (!has_frame) {

	bytes_read = g_input_stream_read (input_stream,
					  buffer,
					  sizeof (buffer),
					  NULL,
					  NULL);
	if (bytes_read == -1) {
	    break;
	}
	result = TRUE;
	if (bytes_read == 0) {
	    break;
	}

	if (!gdk_pixbuf_loader_write (loader,
				      (unsigned char *)buffer,
				      bytes_read,
				      NULL)) {
	    result = FALSE;
	    break;
	}

	animation = gdk_pixbuf_loader_get_animation (loader);
	if (animation) {
		iter = gdk_pixbuf_animation_get_iter (animation, NULL);
		if (!gdk_pixbuf_animation_iter_on_currently_loading_frame (iter)) {
			has_frame = TRUE;
		}
		g_object_unref (iter);
	}
    }

    gdk_pixbuf_loader_close (loader, NULL);

    if (!result) {
	g_object_unref (G_OBJECT (loader));
	g_input_stream_close (input_stream, NULL, NULL);
	g_object_unref (input_stream);
	g_object_unref (file);
	return NULL;
    }

    g_input_stream_close (input_stream, NULL, NULL);
    g_object_unref (input_stream);
    g_object_unref (file);

    pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
    if (pixbuf != NULL) {
	g_object_ref (G_OBJECT (pixbuf));
	g_object_set_data (G_OBJECT (pixbuf), "gnome-original-width",
			   GINT_TO_POINTER (info.input_width));
	g_object_set_data (G_OBJECT (pixbuf), "gnome-original-height",
			   GINT_TO_POINTER (info.input_height));
    }
    g_object_unref (G_OBJECT (loader));

    return pixbuf;
}

GdkPixbuf *
desktop_thumbnail_factory_generate_thumbnail (GHashTable              *scripts_hash,
						    const char              *uri,
						    const char              *mime_type)
{
  GdkPixbuf *pixbuf, *scaled, *tmp_pixbuf;
  char *script, *expanded_script;
  int width, height, size;
  int original_width = 0;
  int original_height = 0;
  char dimension[12];
  double scale;
  int exit_status;
  char *tmpname;

  g_return_val_if_fail (uri != NULL, NULL);
  g_return_val_if_fail (mime_type != NULL, NULL);

  size = 128;

  pixbuf = NULL;

  script = NULL;
  if (scripts_hash)
    {
      script = g_hash_table_lookup (scripts_hash, mime_type);
      if (script)
        script = g_strdup (script);
    }

  if (script)
    {
      int fd;

      fd = g_file_open_tmp (".gnome_desktop_thumbnail.XXXXXX", &tmpname, NULL);

      if (fd != -1)
	{
	  close (fd);

	  expanded_script = expand_thumbnailing_script (script, size, uri, tmpname);
	  if (expanded_script != NULL &&
	      g_spawn_command_line_sync (expanded_script,
					 NULL, NULL, &exit_status, NULL) &&
	      exit_status == 0)
	    {
	      pixbuf = gdk_pixbuf_new_from_file (tmpname, NULL);
	    }

	  g_free (expanded_script);
	  g_unlink (tmpname);
	  g_free (tmpname);
	}

      g_free (script);
    }

  /* Fall back to gdk-pixbuf */
  if (pixbuf == NULL)
    {
      pixbuf = _gdk_pixbuf_new_from_uri_at_scale (uri, size, size, TRUE);

      if (pixbuf != NULL)
        {
          original_width = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (pixbuf),
                                                               "gnome-original-width"));
          original_height = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (pixbuf),
                                                                "gnome-original-height"));
        }
    }
      
  if (pixbuf == NULL)
    return NULL;

  /* The pixbuf loader may attach an "orientation" option to the pixbuf,
     if the tiff or exif jpeg file had an orientation tag. Rotate/flip
     the pixbuf as specified by this tag, if present. */
  tmp_pixbuf = gdk_pixbuf_apply_embedded_orientation (pixbuf);
  g_object_unref (pixbuf);
  pixbuf = tmp_pixbuf;

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);
  
  if (width > size || height > size)
    {
      const gchar *orig_width, *orig_height;
      scale = (double)size / MAX (width, height);

      scaled = gnome_desktop_thumbnail_scale_down_pixbuf (pixbuf,
						  floor (width * scale + 0.5),
						  floor (height * scale + 0.5));

      orig_width = gdk_pixbuf_get_option (pixbuf, "tEXt::Thumb::Image::Width");
      orig_height = gdk_pixbuf_get_option (pixbuf, "tEXt::Thumb::Image::Height");

      if (orig_width != NULL) {
	      gdk_pixbuf_set_option (scaled, "tEXt::Thumb::Image::Width", orig_width);
      }
      if (orig_height != NULL) {
	      gdk_pixbuf_set_option (scaled, "tEXt::Thumb::Image::Height", orig_height);
      }
      
      g_object_unref (pixbuf);
      pixbuf = scaled;
    }
  
  if (original_width > 0) {
	  g_snprintf (dimension, sizeof (dimension), "%i", original_width);
	  gdk_pixbuf_set_option (pixbuf, "tEXt::Thumb::Image::Width", dimension);
  }
  if (original_height > 0) {
	  g_snprintf (dimension, sizeof (dimension), "%i", original_height);
	  gdk_pixbuf_set_option (pixbuf, "tEXt::Thumb::Image::Height", dimension);
  }

  return pixbuf;
}

static void
update_preview (GtkFileChooser               *chooser,
                GHashTable                   *scripts_hash)
{
        gchar *uri;

        uri = gtk_file_chooser_get_uri (chooser);

        if (uri) {
                GdkPixbuf *pixbuf = NULL;
                const gchar *mime_type = NULL;
                GFile *file;
                GFileInfo *file_info;
                GtkWidget *preview;

                preview = gtk_file_chooser_get_preview_widget (chooser);

                file = g_file_new_for_uri (uri);
                file_info = g_file_query_info (file,
                                               "standard::*",
                                               G_FILE_QUERY_INFO_NONE,
                                               NULL, NULL);
                g_object_unref (file);

                if (file_info != NULL &&
                    g_file_info_get_file_type (file_info) != G_FILE_TYPE_DIRECTORY) {
                        mime_type = g_file_info_get_content_type (file_info);
                        g_object_unref (file_info);
                }

                if (mime_type) {
                        pixbuf = desktop_thumbnail_factory_generate_thumbnail (scripts_hash,
                                                                               uri,
                                                                               mime_type);
                }

                gtk_dialog_set_response_sensitive (GTK_DIALOG (chooser),
                                                   GTK_RESPONSE_ACCEPT,
                                                   (pixbuf != NULL));

                if (pixbuf != NULL) {
                        gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
                        g_object_unref (pixbuf);
                }
                else {
                        gtk_image_set_from_icon_name (GTK_IMAGE (preview),
                                                      "dialog-question",
                                                      GTK_ICON_SIZE_DIALOG);
                }

                g_free (uri);
        }

        gtk_file_chooser_set_preview_widget_active (chooser, TRUE);
}

static void
um_photo_dialog_select_file (UmPhotoDialog *um)
{
        GtkWidget *chooser;
        const gchar *folder;
        GtkWidget *preview;
        GtkFileFilter *filter;

        chooser = gtk_file_chooser_dialog_new (_("Browse for more pictures"),
                                               GTK_WINDOW (gtk_widget_get_toplevel (um->popup_button)),
                                               GTK_FILE_CHOOSER_ACTION_OPEN,
                                               _("_Cancel"), GTK_RESPONSE_CANCEL,
                                               _("_Open"), GTK_RESPONSE_ACCEPT,
                                               NULL);

        gtk_window_set_modal (GTK_WINDOW (chooser), TRUE);

        preview = gtk_image_new ();
        gtk_widget_set_size_request (preview, 128, -1);
        gtk_file_chooser_set_preview_widget (GTK_FILE_CHOOSER (chooser), preview);
        gtk_file_chooser_set_use_preview_label (GTK_FILE_CHOOSER (chooser), FALSE);
        gtk_widget_show (preview);

        /* Preview has to be generated after default handler of "selection-changed"
         * signal, otherwise dialog response sensitivity is rewritten (Bug 547988).
         * Preview also has to be generated on "selection-changed" signal to reflect
         * all changes (Bug 660877). */
        g_signal_connect_after (chooser, "selection-changed",
                                G_CALLBACK (update_preview), NULL);

        folder = g_get_user_special_dir (G_USER_DIRECTORY_PICTURES);
        if (folder)
                gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser),
                                                     folder);

        filter = gtk_file_filter_new ();
        gtk_file_filter_add_pixbuf_formats (filter);
        gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (chooser), filter);

        g_signal_connect (chooser, "response",
                          G_CALLBACK (file_chooser_response), um);

        gtk_window_present (GTK_WINDOW (chooser));
}

static void
none_icon_selected (GtkMenuItem   *menuitem,
                    UmPhotoDialog *um)
{
        act_user_set_icon_file (um->user, "");
}

static void
file_icon_selected (GtkMenuItem   *menuitem,
                    UmPhotoDialog *um)
{
        um_photo_dialog_select_file (um);
}

#ifdef HAVE_CHEESE
static gboolean
destroy_chooser (GtkWidget *chooser)
{
        gtk_widget_destroy (chooser);
        return FALSE;
}

static void
webcam_response_cb (GtkDialog     *dialog,
                    int            response,
                    UmPhotoDialog  *um)
{
        if (response == GTK_RESPONSE_ACCEPT) {
                GdkPixbuf *pb, *pb2;

                g_object_get (G_OBJECT (dialog), "pixbuf", &pb, NULL);
                pb2 = gdk_pixbuf_scale_simple (pb, 96, 96, GDK_INTERP_BILINEAR);

                set_user_icon_data (um->user, pb2);

                g_object_unref (pb2);
                g_object_unref (pb);
        }
        if (response != GTK_RESPONSE_DELETE_EVENT &&
            response != GTK_RESPONSE_NONE)
                g_idle_add ((GSourceFunc) destroy_chooser, dialog);
}

static void
webcam_icon_selected (GtkMenuItem   *menuitem,
                      UmPhotoDialog *um)
{
        GtkWidget *window;

        window = cheese_avatar_chooser_new ();
        gtk_window_set_transient_for (GTK_WINDOW (window),
                                      GTK_WINDOW (gtk_widget_get_toplevel (um->popup_button)));
        gtk_window_set_modal (GTK_WINDOW (window), TRUE);
        g_signal_connect (G_OBJECT (window), "response",
                          G_CALLBACK (webcam_response_cb), um);
        gtk_widget_show (window);
}

static void
update_photo_menu_status (UmPhotoDialog *um)
{
        if (um->num_cameras == 0)
                gtk_widget_set_sensitive (um->take_photo_menuitem, FALSE);
        else
                gtk_widget_set_sensitive (um->take_photo_menuitem, TRUE);
}

static void
device_added (CheeseCameraDeviceMonitor *monitor,
              CheeseCameraDevice        *device,
              UmPhotoDialog             *um)
{
        um->num_cameras++;
        update_photo_menu_status (um);
}

static void
device_removed (CheeseCameraDeviceMonitor *monitor,
                const char                *id,
                UmPhotoDialog             *um)
{
        um->num_cameras--;
        update_photo_menu_status (um);
}

#endif /* HAVE_CHEESE */

static void
stock_icon_selected (GtkMenuItem   *menuitem,
                     UmPhotoDialog *um)
{
        const char *filename;

        filename = g_object_get_data (G_OBJECT (menuitem), "filename");
        act_user_set_icon_file (um->user, filename);
}

static GtkWidget *
menu_item_for_filename (UmPhotoDialog *um,
                        const char    *filename)
{
        GtkWidget *image, *menuitem;
        GFile *file;
        GIcon *icon;

        file = g_file_new_for_path (filename);
        icon = g_file_icon_new (file);
        g_object_unref (file);
        image = gtk_image_new_from_gicon (icon, GTK_ICON_SIZE_DIALOG);
        g_object_unref (icon);

        menuitem = gtk_menu_item_new ();
        gtk_container_add (GTK_CONTAINER (menuitem), image);
        gtk_widget_show_all (menuitem);

        g_object_set_data_full (G_OBJECT (menuitem), "filename",
                                g_strdup (filename), (GDestroyNotify) g_free);
        g_signal_connect (G_OBJECT (menuitem), "activate",
                          G_CALLBACK (stock_icon_selected), um);

        return menuitem;
}

static void
setup_photo_popup (UmPhotoDialog *um)
{
        GtkWidget *menu, *menuitem, *image;
        guint x, y;
        const gchar * const * dirs;
        guint i;
        GDir *dir;
        const char *face;
        gboolean none_item_shown;
        gboolean added_faces;

        menu = gtk_menu_new ();

        x = 0;
        y = 0;
        none_item_shown = added_faces = FALSE;

        dirs = g_get_system_data_dirs ();
        for (i = 0; dirs[i] != NULL; i++) {
                char *path;

                path = g_build_filename (dirs[i], "pixmaps", "faces", NULL);
                dir = g_dir_open (path, 0, NULL);
                if (dir == NULL) {
                        g_free (path);
                        continue;
                }

                while ((face = g_dir_read_name (dir)) != NULL) {
                        char *filename;

                        added_faces = TRUE;

                        filename = g_build_filename (path, face, NULL);
                        menuitem = menu_item_for_filename (um, filename);
                        g_free (filename);
                        if (menuitem == NULL)
                                continue;

                        gtk_menu_attach (GTK_MENU (menu), GTK_WIDGET (menuitem),
                                         x, x + 1, y, y + 1);
                        gtk_widget_show (menuitem);

                        x++;
                        if (x >= ROW_SPAN - 1) {
                                y++;
                                x = 0;
                        }
                }
                g_dir_close (dir);
                g_free (path);

                if (added_faces)
                        break;
        }

        if (!added_faces)
                goto skip_faces;

        image = gtk_image_new_from_icon_name ("avatar-default", GTK_ICON_SIZE_DIALOG);
        menuitem = gtk_menu_item_new ();
        gtk_container_add (GTK_CONTAINER (menuitem), image);
        gtk_widget_show_all (menuitem);
        gtk_menu_attach (GTK_MENU (menu), GTK_WIDGET (menuitem),
                         x, x + 1, y, y + 1);
        g_signal_connect (G_OBJECT (menuitem), "activate",
                          G_CALLBACK (none_icon_selected), um);
        gtk_widget_show (menuitem);
        none_item_shown = TRUE;
        y++;

skip_faces:
        if (!none_item_shown) {
                menuitem = gtk_menu_item_new_with_label (_("Disable image"));
                gtk_menu_attach (GTK_MENU (menu), GTK_WIDGET (menuitem),
                                 0, ROW_SPAN - 1, y, y + 1);
                g_signal_connect (G_OBJECT (menuitem), "activate",
                                  G_CALLBACK (none_icon_selected), um);
                gtk_widget_show (menuitem);
                y++;
        }

        /* Separator */
        menuitem = gtk_separator_menu_item_new ();
        gtk_menu_attach (GTK_MENU (menu), GTK_WIDGET (menuitem),
                         0, ROW_SPAN - 1, y, y + 1);
        gtk_widget_show (menuitem);

        y++;

#ifdef HAVE_CHEESE
        um->take_photo_menuitem = gtk_menu_item_new_with_label (_("Take a photo…"));
        gtk_menu_attach (GTK_MENU (menu), GTK_WIDGET (um->take_photo_menuitem),
                         0, ROW_SPAN - 1, y, y + 1);
        g_signal_connect (G_OBJECT (um->take_photo_menuitem), "activate",
                          G_CALLBACK (webcam_icon_selected), um);
        gtk_widget_set_sensitive (um->take_photo_menuitem, FALSE);
        gtk_widget_show (um->take_photo_menuitem);

        um->monitor = cheese_camera_device_monitor_new ();
        g_signal_connect (G_OBJECT (um->monitor), "added",
                          G_CALLBACK (device_added), um);
        g_signal_connect (G_OBJECT (um->monitor), "removed",
                          G_CALLBACK (device_removed), um);
        cheese_camera_device_monitor_coldplug (um->monitor);

        y++;
#endif /* HAVE_CHEESE */

        menuitem = gtk_menu_item_new_with_label (_("Browse for more pictures…"));
        gtk_menu_attach (GTK_MENU (menu), GTK_WIDGET (menuitem),
                         0, ROW_SPAN - 1, y, y + 1);
        g_signal_connect (G_OBJECT (menuitem), "activate",
                          G_CALLBACK (file_icon_selected), um);
        gtk_widget_show (menuitem);

        um->photo_popup = menu;
}

static void
popup_icon_menu (GtkToggleButton *button, UmPhotoDialog *um)
{
        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)) && !gtk_widget_get_visible (um->photo_popup)) {
                gtk_menu_popup (GTK_MENU (um->photo_popup),
                                NULL, NULL,
                                (GtkMenuPositionFunc) popup_menu_below_button, um->popup_button,
                                0, gtk_get_current_event_time ());
        } else {
                gtk_menu_popdown (GTK_MENU (um->photo_popup));
        }
}

static gboolean
on_popup_button_button_pressed (GtkToggleButton *button,
                                GdkEventButton *event,
                                UmPhotoDialog  *um)
{
        if (event->button == 1) {
                if (!gtk_widget_get_visible (um->photo_popup)) {
                        popup_icon_menu (button, um);
                        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
                } else {
                        gtk_menu_popdown (GTK_MENU (um->photo_popup));
                        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), FALSE);
                }

                return TRUE;
        }

        return FALSE;
}

static void
on_photo_popup_unmap (GtkWidget     *popup_menu,
                      UmPhotoDialog *um)
{
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (um->popup_button), FALSE);
}

static void
popup_button_draw (GtkWidget      *widget,
                   cairo_t        *cr,
                   UmPhotoDialog  *um)
{
        if ((gtk_widget_get_state (gtk_bin_get_child (GTK_BIN (widget))) != GTK_STATE_PRELIGHT) &&
            !gtk_widget_is_focus (widget)) {
                return;
        }

        GtkAllocation allocation;
        gtk_widget_get_allocation (widget, &allocation);
        down_arrow (gtk_widget_get_style (widget),
                    cr,
                    allocation.width - 12,
                    allocation.height - 12,
                    12, 12);
}

static void
popup_button_focus_changed (GObject       *button,
                            GParamSpec    *pspec,
                            UmPhotoDialog *um)
{
        gtk_widget_queue_draw (gtk_bin_get_child (GTK_BIN (button)));
}

UmPhotoDialog *
um_photo_dialog_new (GtkWidget *button)
{
        UmPhotoDialog *um;

        um = g_new0 (UmPhotoDialog, 1);

        /* Set up the popup */
        um->popup_button = button;
        setup_photo_popup (um);
        g_signal_connect (button, "toggled",
                          G_CALLBACK (popup_icon_menu), um);
        g_signal_connect (button, "button-press-event",
                          G_CALLBACK (on_popup_button_button_pressed), um);
        g_signal_connect (button, "notify::is-focus",
                          G_CALLBACK (popup_button_focus_changed), um);
        g_signal_connect_after (button, "draw",
                                G_CALLBACK (popup_button_draw), um);

        g_signal_connect (um->photo_popup, "unmap",
                          G_CALLBACK (on_photo_popup_unmap), um);

        return um;
}

void
um_photo_dialog_free (UmPhotoDialog *um)
{
        gtk_widget_destroy (um->photo_popup);

#ifdef HAVE_CHEESE
        if (um->monitor)
                g_object_unref (um->monitor);
#endif
        if (um->user)
                g_object_unref (um->user);

        g_free (um);
}

static void
clear_tip (GtkMenuItem  *item,
           gpointer      user_data)
{
        GList *children;
        GtkWidget *image;
        GIcon *icon, *icon2;
        const char *filename;

        /* Not a stock icon? */
        filename = g_object_get_data (G_OBJECT (item), "filename");
        if (filename == NULL)
                return;

        children = gtk_container_get_children (GTK_CONTAINER (item));
        image = children->data;
        g_assert (image != NULL);
        g_list_free (children);

        gtk_image_get_gicon (GTK_IMAGE (image), &icon, NULL);

        if (G_IS_EMBLEMED_ICON (icon))
                icon2 = g_emblemed_icon_get_icon (G_EMBLEMED_ICON (icon));
        else
                return;

        gtk_image_set_from_gicon (GTK_IMAGE (image), icon2, GTK_ICON_SIZE_DIALOG);
        g_object_unref (icon);
}

static void
set_tip (GtkWidget  *item,
         const char *tip,
         GEmblem    *emblem)
{
        GList *children;
        GtkWidget *image;
        GIcon *icon, *icon2;

        children = gtk_container_get_children (GTK_CONTAINER (item));
        image = children->data;
        g_assert (image != NULL);
        g_list_free (children);

        gtk_image_get_gicon (GTK_IMAGE (image), &icon, NULL);
        if (G_IS_EMBLEMED_ICON (icon)) {
                return;
        }

        icon2 = g_emblemed_icon_new (icon, emblem);
        gtk_image_set_from_gicon (GTK_IMAGE (image), icon2, GTK_ICON_SIZE_DIALOG);

        gtk_widget_set_tooltip_text (GTK_WIDGET (item), tip);
}

void
um_photo_dialog_set_user (UmPhotoDialog *um,
                          ActUser       *user)
{
        ActUserManager *manager;
        GSList *list, *l;
        ActUser *u;
        GIcon *icon;
        GEmblem *emblem;
        GList *children, *c;

        g_return_if_fail (um != NULL);

        if (um->user) {
                g_object_unref (um->user);
                um->user = NULL;
        }
        um->user = user;

        if (um->user) {
                g_object_ref (um->user);

                children = gtk_container_get_children (GTK_CONTAINER (um->photo_popup));
                g_list_foreach (children, (GFunc) clear_tip, NULL);

                manager = act_user_manager_get_default ();
                list = act_user_manager_list_users (manager);

                icon = g_themed_icon_new ("avatar-default");
                emblem = g_emblem_new (icon);
                g_object_unref (icon);

                for (l = list; l; l = l->next) {
                        const char *filename;

                        u = l->data;
                        if (u == user)
                                continue;
                        filename = act_user_get_icon_file (u);
                        if (filename  == NULL)
                                continue;
                        for (c = children; c; c = c->next) {
                                const char *f;

                                f = g_object_get_data (G_OBJECT (c->data), "filename");
                                if (f == NULL)
                                        continue;
                                if (strcmp (f, filename) == 0) {
                                        char *tip;

                                        tip = g_strdup_printf (_("Used by %s"),
                                                               act_user_get_real_name (u));
                                        set_tip (GTK_WIDGET (c->data), tip, emblem);
                                        g_free (tip);
                                        break;
                                }
                        }
                }
                g_slist_free (list);

                g_object_unref (emblem);
        }
}

