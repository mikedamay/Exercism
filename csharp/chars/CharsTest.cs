using Xunit;

public class CharsTest
{
    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_empty_string()
    {
        Assert.Equal(string.Empty, CharUtils.CleanIdentifier(string.Empty));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_single_letter()
    {
        Assert.Equal("A", CharUtils.CleanIdentifier("A"));
    }

    [Fact(/*Skip = "Remove this Skip property to run this test"*/)]
    public void Convert_kebab_to_camel_case()
    {
        Assert.Equal("àḂç", CharUtils.CleanIdentifier("à-ḃç"));
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
        Assert.Equal("my___Id", CharUtils.CleanIdentifier("my   Id"));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Clean_string_with_no_letters()
    {
        Assert.Equal(string.Empty, CharUtils.CleanIdentifier("😀😀😀"));
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
    public void Insert_char_into_string_with_outlying_char()
    {
        Assert.Equal("ǞÊÆ", CharUtils.InsertCharacter("ǞÊ", 'Æ'));
    }

    [Fact( /*Skip = "Remove this Skip property to run this test"*/)]
    public void Insert_ascii_char_into_string()
    {
        Assert.Equal("AmZ", CharUtils.InsertCharacter("AZ", 'm'));
    }

}
