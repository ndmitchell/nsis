
-- | Plugin to change how many sections can be selected at once.
module Development.NSIS.Plugins.Sections(atMostOneSection, exactlyOneSection) where

import Control.Monad
import Development.NSIS


atMostOneSection :: [SectionId] -> Action ()
atMostOneSection xs = do
    selected <- mutableInt_ (-1)
    let ensure = do
            -- find the lowest selected, which isn't already equal to selected
            prev <- constant_ selected
            forM_ (reverse $ zip [0..] xs) $ \(i,x) -> do
                iff_ (prev %/= int i %&& sectionGet x SF_Selected) $ do
                    selected @= int i

            -- ensure only (at most) selected is actually selected
            forM_ (zip [0..] xs) $ \(i,x) -> do
                iff_ (selected %/= int i %&& sectionGet x SF_Selected) $
                    sectionSet x SF_Selected false
    ensure
    onSelChange ensure

exactlyOneSection :: [SectionId] -> Action ()
exactlyOneSection xs = do
    selected <- mutableInt_ (-1)

    let ensure = do
            -- find the lowest selected, which isn't already equal to selected
            prev <- constant_ selected
            forM_ (reverse $ zip [0..] xs) $ \(i,x) ->
                iff_ (prev %/= int i %&& sectionGet x SF_Selected) $
                    selected @= int i
            iff_ (selected %== (-1)) $
                selected @= 0

            -- ensure only selected is actually selected
            forM_ (zip [0..] xs) $ \(i,x) -> do
                let b = selected %== int i
                iff_ (b %/= sectionGet x SF_Selected) $
                    sectionSet x SF_Selected b

    ensure
    onSelChange ensure

