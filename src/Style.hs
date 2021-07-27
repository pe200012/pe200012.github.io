{-# LANGUAGE OverloadedStrings #-}
module Style where

import           Clay
import           Prelude                 hiding ( (**) )

index :: Css
index = do
    body ? do
        margin (px 60) auto (px 60) auto
        width (pct 70)
    nav ** ul ? listCss
    footer ** ul ? listCss
    nav ** ul ** li ? listItemCss
    footer ** ul ** li ? listItemCss
    a ? do
        textDecoration none
        color (rgb 153 153 153) -- rgb hex #999
    a # hover ? textDecoration underline
    h1 ? do
        fontSize (em 3)
        fontFamily ["Helvetica", "Arial", "Sans-Serif"] [sansSerif, monospace]
    p ? do
        fontSize (em 1.5)
        lineHeight (em 1.4)
        color (rgb 51 51 51) -- rgb hex #333
    footer ? do
        borderTop solid (px 1) (rgb 213 213 213) -- rgb hex #d5d5d5
    ul # ".posts" ? do
        margin (px 20) auto (px 40) auto
        fontSize (em 0.8)
    (ul # ".posts") ** li ? listStyle none none none
  where
    listCss = do
        fontFamily ["Source Han Sans", "Helvetica", "Arial", "Sans-Serif"] [sansSerif, monospace]
        padding (px 0) (px 0) (px 0) (px 0)
        listStyle none none none
        fontWeight bold
    listItemCss = do
        display inline
        marginRight (px 20)

postCss :: Css
postCss = do
    star ? do
        position relative
        boxSizing borderBox
    body ? do
        padding (pct 2) auto (pct 2) auto
        marginLeft auto
        marginRight auto
        width (pct 80)
        fontFamily ["Source Han Sans", "Helvetica", "Arial", "Sans-Serif"] [sansSerif, monospace]
    nav ** ul ? listCss
    footer ** ul ? listCss
    nav ** ul ** li ? listItemCss
    footer ** ul ** li ? listItemCss
    a ? do
        color (rgb 128 0 128)
        textDecoration none
        paddingBottom (px 1)
        borderBottom dashed (px 1) (rgb 0 148 146)
    a # hover ? do
        textDecoration none
        borderBottomStyle solid
    h1 ? do
        fontSize (em 3)
    p ? do
        fontSize (em 1)
        lineHeight (em 1.4)
        color (rgb 51 51 51) -- rgb hex #333
    footer ? do
        borderTop solid (px 1) (rgb 213 213 213) -- rgb hex #d5d5d5
    ul # ".posts" ? do
        margin (px 20) auto (px 40) auto
        fontSize (em 0.8)
    (ul # ".posts") ** li ? listStyle none none none
    p # ".author" ? do
        fontSize (em 1)
        color (rgb 153 153 153)
        important (marginBottom (pct 0))
        important (marginTop (px 4))
        height (px 24)
    "blockquote" ? do
        background (rgb 248 248 248)
        borderLeft solid (em 0.5) (rgb 204 204 204)
        margin (em 0.5) (px 0) (em 0.5) (em 1.2)
        padding (em 0.2) (em 0.4) (em 0.2) (em 0.4)
        overflowX auto
        important (color (rgb 85 85 85)) -- rgb #555
    code ? do
        color (rgb 0 0 0)
        fontFamily
            ["Fira Code", "JetBrains Mono", "Menlo", "Andale Mono", "Monaco", "Consolas", "DejaVu Sans Mono", "Courier", "Lucida Sans Unicode", "monospace"]
            [sansSerif, monospace]
        fontSize (em 0.85)
        backgroundColor (rgb 239 240 241)
        lineHeight (em 1.25)
        borderRadius (em 0.25) (em 0.25) (em 0.25) (em 0.25)
    pre ? do
        wordWrap normal
        padding (px 4) (px 4) (px 4) (px 4)
        overflowX auto
  where
    listCss = do
        fontFamily ["Helvetica", "Arial", "Sans-Serif"] [sansSerif, monospace]
        padding (px 0) (px 0) (px 0) (px 0)
        listStyle none none none
        fontWeight bold
    listItemCss = do
        display inline
        marginRight (px 20)


