-- | Disqus-like comment module (WIP)
module Html.Elements.Comments where
{-
createBody :: T'.Text -> H.Html
createBody = H.toHtml . commonmarkToHtml [] [extTable, extAutolink] -- no need for tagfilter extension; we'll sanitize where needed

-- | js that dynamically generates reply comment boxes (we don't want any redundancies sent over the wire!)
-- TODO: use ~/programming/DataMunge.hs method to store this <script> in the generated HTML, having some js duplicate it for any given post (replying to a post, with ?r=postnum) or making a non-reply comment (?r=)
commentJS :: Html ()
commentJS = with form_ [action_ "javascript:postComment()"] $ do -- postComment will AJAX POST to /comment; comment will return true or false, or timeout
        with textarea_ [name_ "c"] -- see google search "textarea div"...?
        with input_ [type_ "text", name_ "n"]
        with input_ [type_ "text", name_ "e"]
        with input_ [type_ "text", name_ "w"]
        with input_ [type_ "submit", value_ "Submit"]

commentModule :: Post -> Html ()
commentModule = do
    -- 1st, the comment box
    em_ $ (T'.pack $ show numComments) <> "Comments"
    -- next, the comment tree expressed as HTML (:: Tree Comment -> H.Html)
    snd <$> foldM f (mempty,0) commentTree -- is foldM appropriate here? (is the traversal breadth- or depth-first?)
    -- for now, just have one instance of the blog running, and keep storage in serialized files on VM
    where
        f :: MonadIO m => (Html (), Int) -> Comment -> m (Html (), Int)
        f (h,i) (Comment {comment, author, email, time, uid}) = with div_ [makeAttribute "indent" (show i)] $ do
            with span_ [makeAttribute "cuid" (show uid)] $ do
                em_ $ (if T.null author then mempty else author)
                (if T.null email then mempty else " (" <> email <> ")")
                " • "
                timeSince time
            p_ comment
            -- TODO: create toggle function that either hides, shows, or creates & shows the comment box form
            with a_ [href_ "#", onclick_ "displayAddCommentBox(this); this.setText('Close');" $ "Reply"]
        timeSince :: Calendar -> T.Text
        timeSince t = -- x [hours|minutes] ago, or if less than a minute, "just now"
-}
