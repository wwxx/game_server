-module(secure).

-export([encrypt/3, decrypt/3]).

%% Key: 32 byte, IVec: 16 byte
-spec(encrypt(Key::binary()|iolist(), IVec::binary(), Text::binary()) -> Cipher::binary()).
encrypt(Key, IVec, Text) ->
    PadedText = pkcs7:pad(Text),
    crypto:block_encrypt(aes_cbc256, Key, IVec, PadedText).

-spec(decrypt(Key::binary()|iolist(), IVec::binary(), Cipher::binary()) -> Text::binary()).
decrypt(Key, IVec, Cipher) ->
    Text = crypto:block_decrypt(aes_cbc256, Key, IVec, Cipher),
    pkcs7:unpad(Text).
