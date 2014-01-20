-module(secure).

-export([encrypt/3, decrypt/3]).

-spec(encrypt(Key::binary()|iolist(), IVec::binary(), Text::binary()) -> Cipher::binary()).
%% Key: 32 byte, IVec, 16 byte
encrypt(Key, IVec, Text) ->
    ZippedText = zlib:gzip(Text),
    PadedText = pkcs7:pad(ZippedText),
    Cipher = crypto:block_encrypt(aes_cbc256, Key, IVec, PadedText),
    base64:encode(Cipher).

-spec(decrypt(Key::binary()|iolist(), IVec::binary(), Cipher::binary()) -> Text::binary()).
decrypt(Key, IVec, Cipher) ->
    Text = crypto:block_decrypt(aes_cbc256, Key, IVec, Cipher),
    pkcs7:unpad(Text).
