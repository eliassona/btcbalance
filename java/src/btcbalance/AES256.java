package btcbalance;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

public class AES256 {
   private static final String ALGO = "AES";

   public static String encrypt(final String masterKey, final String data) throws Exception {
       final Key key = generateKey(masterKey);
       final Cipher c = Cipher.getInstance(ALGO);
       c.init(Cipher.ENCRYPT_MODE, key);
       final byte[] encVal = c.doFinal(data.getBytes());
       final String encryptedValue = Base64.getEncoder().encodeToString(encVal);
       return encryptedValue;
   }

   public static String decrypt(final String masterKey, final String encryptedData) throws Exception {
       final Key key = generateKey(masterKey);
       final Cipher c = Cipher.getInstance(ALGO);
       c.init(Cipher.DECRYPT_MODE, key);
       final byte[] decordedValue = Base64.getDecoder().decode(encryptedData);
       final byte[] decValue = c.doFinal(decordedValue);
       final String decryptedValue = new String(decValue);
       return decryptedValue;
   }
   
   public static byte[] sha256(final String str) throws NoSuchAlgorithmException {
	   final MessageDigest digest = MessageDigest.getInstance("SHA-256");
       return digest.digest(str.getBytes(StandardCharsets.UTF_8));
   }

   private static Key generateKey(final String masterKey) throws Exception {
       final Key key = new SecretKeySpec(sha256(masterKey), ALGO);
       return key;
   }
}
