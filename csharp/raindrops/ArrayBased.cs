public class ArrayBased
{
    public static string Convert(int number)
    {
        var dropSounds = string.Empty;
        foreach (var drop in new (int num, string sound)[] {(3, "Pling"), (5, "Plang"), (7, "Plong")})
        {
            if (number % drop.num == 0)
            {
                dropSounds += drop.sound;
            }
        }        
        if (string.IsNullOrEmpty(dropSounds))
        {
            dropSounds = number.ToString();
        }
        return dropSounds;
    
}
