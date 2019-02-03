
public class PangramChecker {

    public boolean isPangram(String input) {
       var chs = "abcdefghijklmnopqrstuvwxyz".toCharArray();
       for (var ch : input.toLowerCase().toCharArray()) {
           if ( ch >= 'a' && ch <= 'z')
               chs[ch - 'a'] = 0;
       }
       for ( var ch : chs) {
           if ( ch != '\0')
               return false;
       }
       return true;
    }
}
