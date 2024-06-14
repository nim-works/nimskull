import std/wordwrap
import openssl/[core_dispatch, bio, decoder, evp, rsa]

const PubKey = r"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAknKWvrdnncCIzBnIGrZ5qtZrPH+Yo3t7ag9WZIu6Gmc/JgIDDaZhJeyGW0YSnifeAEhooWvM4jDWhTEARzktalSHqYtmwI/1Oxwp6NTYH8akMe2LCpZ5pX9FVA6m9o2tkbdXatbDKRqeD4UA8Ow7Iyrdo6eb1SU8vk+26i+uXHTtsb25p8uf2ppOJrJCy+1vr8Gsnuwny1UdoYZTxMsxRFPf+UX/LrSXMHVq/oPVa3SJ4VHMpYrG/httAugVP6K58xiZ93jst63/dd0JL85mWJu1uS3uz92aL5O97xzth3wR4BbdmDUlN4LuTIwi6DtEcC7gUOTnOzH4zgp2b5RyHwIDAQAB"
const PrivateKey = r"MIIEpAIBAAKCAQEAknKWvrdnncCIzBnIGrZ5qtZrPH+Yo3t7ag9WZIu6Gmc/JgIDDaZhJeyGW0YSnifeAEhooWvM4jDWhTEARzktalSHqYtmwI/1Oxwp6NTYH8akMe2LCpZ5pX9FVA6m9o2tkbdXatbDKRqeD4UA8Ow7Iyrdo6eb1SU8vk+26i+uXHTtsb25p8uf2ppOJrJCy+1vr8Gsnuwny1UdoYZTxMsxRFPf+UX/LrSXMHVq/oPVa3SJ4VHMpYrG/httAugVP6K58xiZ93jst63/dd0JL85mWJu1uS3uz92aL5O97xzth3wR4BbdmDUlN4LuTIwi6DtEcC7gUOTnOzH4zgp2b5RyHwIDAQABAoIBACSOxmLFlfAjaALLTNCeTLEA5bQshgYJhT1sprxixQpiS7lJN0npBsdYzBFs5KjmetzHNpdVOcgdOO/204L0Gwo4H8WLLxNS3HztAulEeM813zc3fUYfWi6eHshk//j8VR/TDNd21TElm99z7FA4KGsXAE0iQhxrN0aqz5aWYIhjprtHA5KxXIiESnTkof5Cud8oXEnPiwPGNhq93QeQzh7xQIKSaDKBcdAa6edTFhzc4RLUQRfrik/GqJzouEDQ9v6H/uiOLTB3FxxwErQIf6dvSVhD9gs1nSLQfyj3S2Hxe9S2zglTl07EsawTQUxtVQkdZUOok67c7CPBxecZ2wECgYEA2c31gr/UJwczT+P/AE52GkHHETXMxqE3Hnh9n4CitfAFSD5X0VwZvGjZIlln2WjisTd92Ymf65eDylX2kCm93nzZ2GfXgS4zl4oY1N87+VeNQlx9f2+6GU7Hs0HFdfu8bGd+0sOuWA1PFqQCobxCACMPTkuzsG9M7knUTN59HS8CgYEArCEoP4ReYoOFveXUE0AteTPb4hryvR9VDEolP+LMoiPe8AzBMeB5fP493TPdjtnWmrPCXNLc7UAFSj2CZsRhau4PuiqnNrsb5iz/7iXVl3E8wZvS4w7WYpO4m33L0cijA6MdcdqilQu4Z5tw4nG45lAW9UYyOc9D4hJTzgtGHhECgYA6QyDoj931brSoK0ocT+DB11Sj4utbOuberMaV8zgTSRhwodSl+WgdAUMMMDRacPcrBrgQiAMSZ15msqYZHEFhEa7Id8arFKvSXquTzf9iDKyJ0unzO/ThLjS3W+GxVNyrdufzA0tQ3IaKfOcDUrOpC7fdbtyrVqqSl4dF5MI9GwKBgQCl3OF6qyOEDDZgsUk1L59h7k3QR6VmBf4e9IeGUxZamvQlHjU/yY1nm1mjgGnbUB/SPKtqZKoMV6eBTVoNiuhQcItpGda9D3mnx+7p3T0/TBd+fJeuwcplfPDjrEktogcq5w/leQc3Ve7gr1EMcwb3r28f8/9L42QHQR/OKODs8QKBgQCFAvxDRPyYg7V/AgD9rt1KzXi4+b3Pls5NXZa2g/w+hmdhHUNxV5IGmHlqFnptGyshgYgQGxMMkW0iJ1j8nLamFnkbFQOp5/UKbdPLRKiB86oPpxsqYtPXucDUqEfcMsp57mD1CpGVODbspogFpSUvQpMECkhvI0XLMbolMdo53g=="

proc rsaPemDecode(key: string, public: bool): ptr EVP_PKEY =
  let mKey =
    if public:
      "-----BEGIN PUBLIC KEY-----\n" & key.wrapWords(64) & "\n-----END PUBLIC KEY-----"
    else:
      "-----BEGIN RSA PRIVATE KEY-----\n" & key.wrapWords(64) & "\n-----END RSA PRIVATE KEY-----"

  let bio = BIO_new_mem_buf(addr mKey[0], mKey.len.cint)
  doAssert bio != nil
  let selection: cint = if public: OSSL_KEYMGMT_SELECT_PUBLIC_KEY else: OSSL_KEYMGMT_SELECT_PRIVATE_KEY
  let dctx = OSSL_DECODER_CTX_new_for_pkey(addr result, input_type = "PEM", input_struct = nil, keytype = "RSA", selection, nil, nil)
  doAssert dctx != nil
  doAssert OSSL_DECODER_from_bio(dctx, bio) == 1
  doAssert result != nil
  OSSL_DECODER_CTX_free(dctx)
  BIO_free_all(bio)

proc rsaPublicEncrypt(fr: string): string =
  let pkey = rsaPemDecode(PubKey, public = true)
  let ectx = EVP_PKEY_CTX_new_from_pkey(nil, pkey, nil)
  doAssert ectx != nil
  doAssert EVP_PKEY_encrypt_init(ectx) == 1
  doAssert EVP_PKEY_CTX_set_rsa_padding(ectx, RSA_PKCS1_PADDING) == 1

  let frdata = cast[ptr UncheckedArray[cuchar]](fr.cstring)
  var outlen: csize_t
  doAssert EVP_PKEY_encrypt(ectx, nil, addr outlen, frdata, fr.len.csize_t) == 1

  result.setLen(outlen)
  let todata = cast[ptr UncheckedArray[cuchar]](result.cstring)
  doAssert EVP_PKEY_encrypt(ectx, todata, addr outlen, frdata, fr.len.csize_t) == 1
  result.setLen(outlen)

  EVP_PKEY_CTX_free(ectx)

proc rasPrivateDecrypt(fr: string): string =
  let pkey = rsaPemDecode(PrivateKey, public = false)
  let ectx = EVP_PKEY_CTX_new_from_pkey(nil, pkey, nil)
  doAssert ectx != nil
  doAssert EVP_PKEY_decrypt_init(ectx) == 1
  doAssert EVP_PKEY_CTX_set_rsa_padding(ectx, RSA_PKCS1_PADDING) == 1

  let frdata = cast[ptr UncheckedArray[cuchar]](fr.cstring)
  var outlen: csize_t
  doAssert EVP_PKEY_decrypt(ectx, nil, addr outlen, frdata, fr.len.csize_t) == 1

  result.setLen(outlen)
  let todata = cast[ptr UncheckedArray[cuchar]](result.cstring)
  doAssert EVP_PKEY_decrypt(ectx, todata, addr outlen, frdata, fr.len.csize_t) == 1
  result.setLen(outlen)

  EVP_PKEY_CTX_free(ectx)

let res = "TEST"
let miwen = rsaPublicEncrypt(res)
let mingwen = rasPrivateDecrypt(miwen)
doAssert mingwen == res

