;;; mastodon-profile-test.el --- Tests for mastodon-profile.el  -*- lexical-binding: nil -*-

(require 'el-mock)

(defconst gargron-profile-json
  '((id . "1")
    (username . "Gargron")
    (acct . "Gargron")
    (display_name . "Eugen")
    (locked . :json-false)
    (bot . :json-false)
    (discoverable . t)
    (group . :json-false)
    (created_at . "2016-03-16T00:00:00.000Z")
    (note . "<p>Developer of Mastodon and administrator of mastodon.social. I post service announcements, development updates, and personal stuff.</p>")
    (url . "https://mastodon.social/@Gargron")
    (avatar . "https://files.mastodon.social/accounts/avatars/000/000/001/original/d96d39a0abb45b92.jpg")
    (avatar_static . "https://files.mastodon.social/accounts/avatars/000/000/001/original/d96d39a0abb45b92.jpg")
    (header . "https://files.mastodon.social/accounts/headers/000/000/001/original/c91b871f294ea63e.png")
    (header_static . "https://files.mastodon.social/accounts/headers/000/000/001/original/c91b871f294ea63e.png")
    (followers_count . 470905)
    (following_count . 451)
    (statuses_count . 70741)
    (last_status_at . "2021-11-14")
    (emojis . [])
    (fields . (((name . "Patreon")
                (value . "<a href=\"https://www.patreon.com/mastodon\" rel=\"me nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://www.</span><span class=\"\">patreon.com/mastodon</span><span class=\"invisible\"></span></a>")
                (verified_at))
               ((name . "Homepage")
                (value . "<a href=\"https://zeonfederated.com\" rel=\"me nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">zeonfederated.com</span><span class=\"invisible\"></span></a>")
                (verified_at . "2019-07-15T18:29:57.191+00:00"))))))

(defconst ccc-profile-json
  '((id . "369027")
    (username . "CCC")
    (acct . "CCC@social.bau-ha.us")
    (display_name . "")
    (locked . :json-false)
    (bot . :json-false)
    (discoverable . :json-false)
    (group . :json-false)
    (created_at . "2018-06-03T00:00:00.000Z")
    (note . "<p><a href=\"https://www.ccc.de/\" rel=\"nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://www.</span><span class=\"\">ccc.de/</span><span class=\"invisible\"></span></a></p>")
    (url . "https://social.bau-ha.us/@CCC")
    (avatar . "https://files.mastodon.social/cache/accounts/avatars/000/369/027/original/6cfeb310f40e041a.jpg")
    (avatar_static . "https://files.mastodon.social/cache/accounts/avatars/000/369/027/original/6cfeb310f40e041a.jpg")
    (header . "https://files.mastodon.social/cache/accounts/headers/000/369/027/original/0d20bef6131b8139.jpg")
    (header_static . "https://files.mastodon.social/cache/accounts/headers/000/369/027/original/0d20bef6131b8139.jpg")
    (followers_count . 2733)
    (following_count . 120)
    (statuses_count . 1357)
    (last_status_at . "2021-11-02")
    (emojis . [])
    (fields . [])))

(defconst gargon-statuses-json
  `(((id . "123456789012345678")
     (created_at . "2021-11-11T11:11:11.111Z")
     (in_reply_to_id)
     (in_reply_to_account_id)
     (sensitive . :json-false)
     (spoiler_text . "")
     (visibility . "public")
     (language)
     (uri . "https://mastodon.social/users/Gargron/statuses/123456789012345678/activity")
     (url . "https://mastodon.social/users/Gargron/statuses/123456789012345678/activity")
     (replies_count . 0)
     (reblogs_count . 0)
     (favourites_count . 0)
     (favourited . :json-false)
     (reblogged . :json-false)
     (muted . :json-false)
     (bookmarked . :json-false)
     (content . "<p>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.</p>")
     (reblog)
     (application)
     (account ,@gargron-profile-json)
     (media_attachments . [])
     (mentions . [])
     (tags . [])
     (emojis . [])
     (card)
     (poll))
    ((id . "107279356043066700")
     (created_at . "2021-11-11T00:00:00.000Z")
     (in_reply_to_id)
     (in_reply_to_account_id)
     (sensitive . :json-false)
     (spoiler_text . "")
     (visibility . "public")
     (language . "en")
     (uri . "https://mastodon.social/users/Gargron/statuses/107279356043066700")
     (url . "https://mastodon.social/@Gargron/107279356043066700")
     (replies_count . 0)
     (reblogs_count . 2)
     (favourites_count . 0)
     (favourited . :json-false)
     (reblogged . :json-false)
     (muted . :json-false)
     (bookmarked . :json-false)
     (content . "<p><span class=\"h-card\"><a href=\"https://social.bau-ha.us/@CCC\" class=\"u-url mention\">@<span>CCC</span></a></span> At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.</p>")
     (reblog)
     (application
      (name . "Web")
      (website))
     (account ,@gargron-profile-json)
     (media_attachments . [])
     (mentions . (((id . "369027")
                   (username . "CCC")
                   (url . "https://social.bau-ha.us/@CCC")
                   (acct . "CCC@social.bau-ha.us"))))
     (tags . [])
     (emojis . [])
     (card)
     (poll))))

(ert-deftest mastodon-profile--add-author-bylines ()
  "Should correctly format short infos about one account.

When formatting Gargon's state we want to see
- the short description of that profile,
- the url of the avatar (yet to be loaded)
- the info attached to the name"
  (with-mock
    ;; Don't start any image loading:
    (mock (mastodon-media--inline-images * *) => nil)
    ;; Let's not do formatting as that makes it hard to not rely on
    ;; window width and reflowing the text.
    (mock (shr-render-region * *) => nil)
    (if (version< emacs-version "27.1")
        (mock (image-type-available-p 'imagemagick) => t)
      (mock (image-transforms-p) => t))

    (with-temp-buffer
      (let ((mastodon-tl--show-avatars t)
            (mastodon-tl--display-media-p t)
            (username (mastodon-tl--unicode-wrap "Eugen"))
            (handle (mastodon-tl--unicode-wrap "@Gargron")))
        (mastodon-profile--format-user (list gargron-profile-json))

        (should
         (equal
          (buffer-substring-no-properties (point-min) (point-max))
          (format "\n  %s (%s)\n<p>Developer of Mastodon and administrator of mastodon.social. I post service announcements, development updates, and personal stuff.</p>\n"
                  username handle)))

        ;; Check the avatar at pos 2
        (should
         (equal
          (get-text-property 2 'media-url)
          "https://files.mastodon.social/accounts/avatars/000/000/001/original/d96d39a0abb45b92.jpg"))
        (should
         (equal
          (get-text-property 2 'media-state)
          'needs-loading))

        ;; Check the byline state
        (should
         (equal
          (get-text-property 4 'byline)
          t))
        (should
         (equal
          (get-text-property 4 'item-id)
          (alist-get 'id gargron-profile-json)))
        (should
         (equal
          (get-text-property 4 'item-json)
          gargron-profile-json))))))

(ert-deftest mastodon-profile--search-account-by-handle--removes-at ()
  "Should ignore a leading at-sign in user handle.

The search will happen as if called without the \"@\"."
  (with-mock
    (mock (mastodon-http--get-json
           "https://instance.url/api/v2/search"
           '(("q" . "gargron")
             ("type" . "accounts"))))
    (let ((mastodon-instance-url "https://instance.url"))
      ;; We don't check anything from the return value. We only care
      ;; that the mocked fetch was called with the expected URL.
      (mastodon-profile--search-account-by-handle "@gargron"))))

(ert-deftest mastodon-profile--search-account-by-handle--filters-out-false-results ()
  "Should ignore results that don't match the searched handle."
  (with-mock
    (mock (mastodon-http--get-json
           "https://instance.url/api/v2/search"
           '(("q" . "Gargron")
             ("type" . "accounts")))
          => `((accounts ,ccc-profile-json ,gargron-profile-json)))
    (let ((mastodon-instance-url "https://instance.url"))
      (should
       (equal
        (mastodon-profile--search-account-by-handle "Gargron")
        gargron-profile-json)))))

(ert-deftest mastodon-profile--search-account-by-handle--filtering-is-case-sensitive ()
  "Should ignore results that don't match the searched handle with exact case.

TODO: We need to decide if this is actually desired or not."
  (with-mock
    (mock (mastodon-http--get-json
           "https://instance.url/api/v2/search"
           '(("q" . "gargron")
             ("type" . "accounts")))
          => `((accounts ,ccc-profile-json ,gargron-profile-json)))

    (let ((mastodon-instance-url "https://instance.url"))
      (should
       (null
        (mastodon-profile--search-account-by-handle "gargron"))))))

(ert-deftest mastodon-profile--account-from-id--correct-url ()
  "Should use the expected url for looking up by account id."
  (with-mock

    (mock (mastodon-http--get-json
           "https://instance.url/api/v1/accounts/1234567"))

    (let ((mastodon-instance-url "https://instance.url"))
      ;; We don't check anything from the return value. We only care
      ;; that the mocked fetch was called with the expected URL.
      (mastodon-profile--account-from-id "1234567"))))

(ert-deftest mastodon-profile--make-author-buffer ()
  "Should set up the buffer as expected for the given author.

This is a far more complicated test as the
mastodon-profile--make-author-buffer function does so much. There
is a bit too much mocking and this may be brittle but it should
help identify when things change unexpectedly.

TODO: Consider separating the data retrieval and the actual
content generation in the function under test."
  (with-mock
    ;; Don't start any image loading:
    (mock (mastodon-media--inline-images * *) => nil)
    ;; (if (version< emacs-version "27.1")
    ;; (mock (image-type-available-p 'imagemagick) => t)
    ;; (mock (image-transforms-p) => t))
    (mock (mastodon-http--get-json * *) ;"https://instance.url/api/v1/accounts/1/statuses"
          =>
          gargon-statuses-json)
    (mock (mastodon-profile--get-statuses-pinned *)
          =>
          ())
    (mock (mastodon-profile--relationships-get *) ;"1")
          =>
          '(((id . "1") (following . :json-false) (showing_reblogs . :json-false) (notifying . :json-false) (followed_by . :json-false) (blocking . :json-false) (blocked_by . :json-false) (muting . :json-false) (muting_notifications . :json-false) (requested . :json-false) (domain_blocking . :json-false) (endorsed . :json-false) (note . ""))))
    ;; Let's not do formatting as that makes it hard to not rely on
    ;; window width and reflowing the text.
    (mock (shr-render-region * *) => nil)
    ;; Don't perform the actual update call at the end.
    ;; (mock (mastodon-tl--timeline *))
    (mock (mastodon-profile--fetch-server-account-settings)
          => '(max_toot_chars 1312 privacy "public" display_name "Eugen" discoverable t locked :json-false bot :json-false sensitive :json-false language ""))

    (mock (mastodon-profile--format-joined-date-string *) => "Joined March 2016")

    (let ((mastodon-tl--show-avatars t)
          (mastodon-tl--display-media-p t)
          (mastodon-instance-url "https://instance.url"))
      (mastodon-profile--make-author-buffer gargron-profile-json)

      (should
       (equal
        (with-current-buffer "*mastodon-Gargron-statuses*"
          (buffer-substring-no-properties (point-min) (point-max)))
        (concat
         "\n"
         "[img] [img] \n"
         "Eugen\n"
         "@Gargron\n"
         " ――――――――――――\n"
         "<p>Developer of Mastodon and administrator of mastodon.social. I post service announcements, development updates, and personal stuff.</p>\n"
         "_ Patreon __ :: <a href=\"https://www.patreon.com/mastodon\" rel=\"me nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://www.</span><span class=\"\">patreon.com/mastodon</span><span class=\"invisible\"></span></a>_ Homepage _ :: <a href=\"https://zeonfederated.com\" rel=\"me nofollow noopener noreferrer\" target=\"_blank\"><span class=\"invisible\">https://</span><span class=\"\">zeonfederated.com</span><span class=\"invisible\"></span></a>"
         "\n"
         "Joined March 2016"
         "\n\n "
         mastodon-tl--horiz-bar
         "\n"
         " TOOTS: 70741 | FOLLOWERS: 470905 | FOLLOWING: 451\n "
         mastodon-tl--horiz-bar
         "\n"
         "\n "
         mastodon-tl--horiz-bar
         "\n"
         "    TOOTS    \n "
         mastodon-tl--horiz-bar
         "\n"
         "\n"
         "<p>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.</p>\n"
         "  Eugen (@Gargron) 2021-11-11 12:11:11\n  "
         mastodon-tl--horiz-bar
         " 0 ⭐ | 0 🔁 | 0 💬\n"
         "\n"
         "\n"
         "<p><span class=\"h-card\"><a href=\"https://social.bau-ha.us/@CCC\" class=\"u-url mention\">@<span>CCC</span></a></span> At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.</p>\n"
         "  Eugen (@Gargron) 2021-11-11 01:00:00\n  "
         mastodon-tl--horiz-bar
         " 0 ⭐ | 2 🔁 | 0 💬\n"
         "\n"
         )))

      ;; Until the function gets refactored this creates a non-temp
      ;; buffer with Gargron's statuses which we want to delete (if
      ;; the tests succeed).
      (kill-buffer))))
