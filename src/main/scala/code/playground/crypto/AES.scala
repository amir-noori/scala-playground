package code.playground.crypto

import javax.crypto.spec.IvParameterSpec

object AES extends App {

  import org.apache.commons.codec.binary.Base64

  import java.security.MessageDigest
  import java.util
  import javax.crypto.Cipher
  import javax.crypto.spec.SecretKeySpec


  object Encryption {
    def encrypt(key: String, value: String): String = {
      val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
      cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key))
      Base64.encodeBase64String(cipher.doFinal(value.getBytes("UTF-8")))
    }

    def decrypt(key: String, encryptedValue: String): String = {
      val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING");
      val secretKey = new SecretKeySpec(key.getBytes, "AES")
      cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0)))

      new String(cipher.doFinal(Base64.decodeBase64(encryptedValue)))
    }

    def keyToSpec(key: String): SecretKeySpec = {
      var keyBytes: Array[Byte] = (SALT + key).getBytes("UTF-8")
      val sha: MessageDigest = MessageDigest.getInstance("SHA-1")
      keyBytes = sha.digest(keyBytes)
      keyBytes = util.Arrays.copyOf(keyBytes, 16)
      new SecretKeySpec(keyBytes, "AES")
    }

    private val SALT: String =
      "jMhKlOuJnM34G6NHkqo9V010GhLAqOpF0BePojHgh1HgNg8^72k"
  }

  println(Encryption.decrypt("eifhe39&$$#558YRt^2760078o(75hgf", "7Rk2ueYbJLtpQt+Awok2CXeKyF85iF+fCuoJ7W7EIAkFyYlwRvpq7j2Z1WKBRM7gwOC35XybQVo4pxzOezgpow=="))

}
