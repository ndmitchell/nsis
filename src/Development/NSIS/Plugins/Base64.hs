
-- | Base64 plugin: <http://nsis.sourceforge.net/Base64_plug-in>
module Development.NSIS.Plugins.Base64(encrypt, decrypt) where

import Development.NSIS


-- | Base64 data encryption.
encrypt :: Exp String -> Exp String
encrypt x = share x $ \x -> do
    plugin "Base64" "Encrypt" [exp_ x, exp_ $ strLength x]
    pop


-- | Base64 decryption. Reverse of 'encrypt'.
decrypt :: Exp String -> Exp String
decrypt x = share x $ \x -> do
    plugin "Base64" "Decrypt" [exp_ x, exp_ $ strLength x]
    pop
