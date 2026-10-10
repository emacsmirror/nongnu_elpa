;;; mastodon-test.el --- Tests for mastodon.el  -*- lexical-binding: nil -*-

(ert-deftest mastodon-fedi-url-p ()
  ;; TODO: /objects, /display, /profile, /p, /u, /comment
  (should
   ;; User handle:
   (mastodon--fedi-url-p
    "https://misskey.noellabo.jp/@shigeru"))
  (should
   ;; User post:
   (mastodon--fedi-url-p
    "https://todon.nl/@admin/117134394057436440"))
  (should
   ;; tag:
   (mastodon--fedi-url-p
    "https://todon.nl/tags/nobot"))
  (should
   ;; we match such handles, but not with crazy codes and multi slashes
   ;; after them, so we went with URI, below (poss stupid):
   ;; Flipboard URL:
   (not
    (mastodon--fedi-url-p
     "https://flipboard.com/@gizmodo/tech-fknh6odjz/-/a-UQ_TmLCbShiYB55LrrrzWQ%3Aa%3A1876139665-%2F0")))
  (should
   ;; Flipboard URI:
   (mastodon--fedi-url-p
    "https://flipboard.com/users/Gizmodo/statuses/UQ_TmLCbShiYB55LrrrzWQ:a:1876139665"))
  (should
   ;; pleroma user
   (mastodon--fedi-url-p
    "https://udongein.xyz/users/duponin"))
  (should
   ;; pleroma post
   (mastodon--fedi-url-p
    "https://udongein.xyz/notice/AcsRkbQxDJbO6T8hbE"))
  (should
   ;; Akkoma post:
   (mastodon--fedi-url-p
    "https://fe.disroot.org/@muppeth/posts/BBD5a3BRt4d2RTDQdU"))
  (should
   ;; Lemmy post:
   (not (mastodon--fedi-url-p
         "https://lemmy.ml/post/53786458")))
  (should
   ;; Lemmy comment:
   (not (mastodon--fedi-url-p
         "https://lemmy.ml/post/53786458/28261605")))
  (should
   ;; lemmy comment 2:
   (mastodon--fedi-url-p
    "https://lemmy.ml/comment/28252098"))
  (should
   ;; Lemmy user:
   (mastodon--fedi-url-p
    "https://lemmy.ml/u/Trying2KnowMyself"))
  (should
   ;; Misskey post:
   (mastodon--fedi-url-p
    "https://misskey.noellabo.jp/notes/as5y0n7y98"))
  (should
   ;; gts post
   (mastodon--fedi-url-p
    "https://aseachange.com/@elena/statuses/01KXDSRTC2KYZ9VSGB219EXPKP"))
  (should
   ;; peertube post (should this not be external?)
   (mastodon--fedi-url-p
    "https://peertube.tv/w/ej3Ye8C4ZQcFyPZLev4x93"))
  (should
   ;; peertube channel (should this not be external?)
   (mastodon--fedi-url-p
    "https://peertube.tv/c/transport_evolved"))
  (should
   ;; collection
   (mastodon--fedi-url-p
    "https://todon.nl/collections/116879312823651190"))
  ;; TODO: Fed.brid.gy:

  ;; Non-fedi links
  (should
   (not (mastodon--fedi-url-p
         "https://x.com/asdf/status/3412352456345")))
  (should
   (not (mastodon--fedi-url-p
         "https://example.com")))
  (should
   (not (mastodon--fedi-url-p
         "https://www.theguardian.com/profile/kitty-sayers"))))
