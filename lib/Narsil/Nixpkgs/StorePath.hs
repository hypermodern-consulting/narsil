{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                         // nixpkgs // store path
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He could see the shape of it now, the way the data folded back on
--    itself, a path through the grid that had been there all along."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Compute the realized @/nix/store@ path of a flake input from its NAR hash,
--   WITHOUT invoking Nix. A flake input is fetched recursively and added as a
--   fixed-output store object named @source@; its path is therefore a pure
--   function of the @narHash@ recorded in flake.lock. This is nix's
--   @makeFixedOutputPath(Recursive, sha256, "source")@ — which for recursive
--   sha256 reduces to @makeStorePath("source", hash, "source")@ — reimplemented
--   here so we can find a project's locked nixpkgs from flake.lock alone.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.StorePath (
  fixedOutputSourcePath,
  base64Decode,
)
where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)

{- | The realized @/nix/store/<hash>-source@ path for a flake input given its
@narHash@ (the @"sha256-<base64>"@ string from flake.lock). 'Nothing' if the
hash is malformed. Pure — no Nix, no IO.
-}
fixedOutputSourcePath :: Text -> Maybe FilePath
fixedOutputSourcePath narHash = do
  b64 <- T.stripPrefix "sha256-" (T.strip narHash)
  raw <- base64Decode b64
  if BS.length raw /= 32 then Nothing else Just (storePath raw)
 where
  storeDir = "/nix/store"
  name = "source"
  storePath raw =
    let fingerprint =
          "source:sha256:" <> hexEncode raw <> ":" <> T.pack storeDir <> ":" <> T.pack name
        digest = SHA256.hash (TE.encodeUtf8 fingerprint)
        compressed = compress20 digest
     in storeDir <> "/" <> T.unpack (nixBase32 compressed) <> "-" <> name

-- | Lowercase hex of a byte string.
hexEncode :: ByteString -> Text
hexEncode = T.pack . concatMap byteHex . BS.unpack
 where
  byteHex b = [hexDigit (b `shiftR` 4), hexDigit (b .&. 0x0f)]
  hexDigit d = "0123456789abcdef" !! fromIntegral d

{- | Fold a hash down to 20 bytes by XOR (nix's @compressHash@): output byte @i@
is the XOR of every input byte at an index @≡ i (mod 20)@.
-}
compress20 :: ByteString -> ByteString
compress20 bs = BS.pack [byteAt i | i <- [0 .. 19]]
 where
  n = BS.length bs
  byteAt i = foldl xor 0 [BS.index bs k | k <- [i, i + 20 .. n - 1]]

{- | Nix's base32: a custom 32-char alphabet (no e/o/u/t), bytes consumed
low-to-high and emitted most-significant char first. 20 bytes → 32 chars.
-}
nixBase32 :: ByteString -> Text
nixBase32 bs = T.pack [charAt n | n <- [nChars - 1, nChars - 2 .. 0]]
 where
  len = BS.length bs
  nChars = (len * 8 - 1) `div` 5 + 1
  alphabet = "0123456789abcdfghijklmnpqrsvwxyz" :: Text
  charAt n =
    let b = n * 5
        i = b `div` 8
        j = b `mod` 8
        lo = fromIntegral (BS.index bs i) `shiftR` j
        hi = if i + 1 < len then fromIntegral (BS.index bs (i + 1)) `shiftL` (8 - j) else 0
        v = (lo .|. hi) .&. 0x1f :: Int
     in alphabet `T.index` v

{- | Decode standard base64 (padding optional/ignored). 'Nothing' on an invalid
character. Small and self-contained — no extra dependency for one short hash.
-}
base64Decode :: Text -> Maybe ByteString
base64Decode txt = do
  sixes <- traverse b64Val (T.unpack (T.filter (/= '=') txt))
  let bits = concatMap (bitsOf 6) sixes
      byteCount = length bits `div` 8
  pure (BS.pack [bitsToByte (take 8 (drop (k * 8) bits)) | k <- [0 .. byteCount - 1]])
 where
  b64Val c
    | c >= 'A' && c <= 'Z' = Just (fromEnum c - fromEnum 'A')
    | c >= 'a' && c <= 'z' = Just (fromEnum c - fromEnum 'a' + 26)
    | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0' + 52)
    | c == '+' = Just 62
    | c == '/' = Just 63
    | otherwise = Nothing
  bitsOf width v = [(v `shiftR` (width - 1 - k)) .&. 1 | k <- [0 .. width - 1]]
  bitsToByte :: [Int] -> Word8
  bitsToByte = fromIntegral . foldl (\acc b -> acc * 2 + b) 0
