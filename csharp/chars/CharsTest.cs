using Xunit;

public class CharsTest
{
    [Fact]
    public void Report_invalid_string()
    {
        Assert.True(CharUtils.DetectInvalidCharComination("abc->def"));
    }

    [Fact(/*Skip = "Remove this Skip property to run this test"*/)]
    public void Report_invalid_empty_string()
    {
        Assert.False(CharUtils.DetectInvalidCharComination(string.Empty));
    }

    [Fact(/*Skip = "Remove this Skip property to run this test"*/)]
    public void Report_invalid_arrow_string()
    {
        Assert.False(CharUtils.DetectInvalidCharComination("----->"));
    }

    [Fact(/*Skip = "Remove this Skip property to run this test"*/)]
    public void Convert_to_upper_case()
    {
        Assert.Equal("CAMELHUMP", CharUtils.ToUpper("camelHump"));
    }

    [Fact(/*Skip = "Remove this Skip property to run this test"*/)]
    public void Convert_to_upper_case_with_numbers()
    {
        Assert.Equal("FROM_1_TO_10", CharUtils.ToUpper("from_1_to_10"));
    }

    [Fact(/*Skip = "Remove this Skip property to run this test"*/)]
    public void Convert_to_upper_case_with_non_ascii()
    {
        Assert.Equal("ÀḂÇ😀", CharUtils.ToUpper("àḃç😀"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_empty_string()
    {
        Assert.Equal(string.Empty, CharUtils.ToUpper(string.Empty));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_clean_string()
    {
        Assert.Equal("àḃç", CharUtils.CleanIdentifier("àḃç"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_string_with_control_char()
    {
        Assert.Equal("myCTRLId", CharUtils.CleanIdentifier("my\0Id"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_string_with_spaces()
    {
        Assert.Equal("myId", CharUtils.CleanIdentifier("my    Id"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_string_with_no_letters()
    {
        Assert.Equal(string.Empty, CharUtils.CleanIdentifier("😀😀😀"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_string_with_initial_digit()
    {
        Assert.Equal("_0ForYou", CharUtils.CleanIdentifier("00ForYou"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_string_with_tricky_start()
    {
        Assert.Equal("CTRL999Id٢", CharUtils.CleanIdentifier("-\0999Id٢"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Insert_char_into_empty_string()
    {
        Assert.Equal("Ǟ", CharUtils.InsertCharacter(string.Empty, 'Ǟ'));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Insert_char_into_string()
    {
        Assert.Equal("αβγ", CharUtils.InsertCharacter("αγ", 'β'));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Insert_char_into_string_with_equal_char()
    {
        Assert.Equal("ǞÊÆ", CharUtils.InsertCharacter("ǞÊ", 'Æ'));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Insert_ascii_char_into_string()
    {
        Assert.Equal("AmZ", CharUtils.InsertCharacter("AZ", 'm'));
    }

}
