module Graphics.Format.Tiled.Internal
    ( constructorTagModifier
    , renderOrderTagModifier
    ) where

import Data.Char (toLower)

constructorTagModifier :: Int -> String -> String
constructorTagModifier n = map toLower . drop n

renderOrderTagModifier :: String -> String
renderOrderTagModifier "RenderOrderRightDown" = "right-down"
renderOrderTagModifier "RenderOrderRightUp"   = "right-up"
renderOrderTagModifier "RenderOrderLeftDown"  = "left-down"
renderOrderTagModifier "RenderOrderLeftUp"    = "left-up"
renderOrderTagModifier _                      = "right-down"
