// This file was auto-generated based on version 1.2.0 of the canonical data.

using Xunit;

public class MarkdownTest
{
    [Fact]
    public void Parses_normal_text_as_a_paragraph()
    {
        var markdown = "This will be a paragraph";
        var expected = "<p>This will be a paragraph</p>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void Parsing_italics()
    {
        var markdown = "_This will be italic_";
        var expected = "<p><em>This will be italic</em></p>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void Parsing_bold_text()
    {
        var markdown = "__This will be bold__";
        var expected = "<p><strong>This will be bold</strong></p>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void Mixed_normal_italics_and_bold_text()
    {
        var markdown = "This will _be_ __mixed__";
        var expected = "<p>This will <em>be</em> <strong>mixed</strong></p>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void With_h1_header_level()
    {
        var markdown = "# This will be an h1";
        var expected = "<h1>This will be an h1</h1>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void With_h2_header_level()
    {
        var markdown = "## This will be an h2";
        var expected = "<h2>This will be an h2</h2>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void With_h6_header_level()
    {
        var markdown = "###### This will be an h6";
        var expected = "<h6>This will be an h6</h6>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void Unordered_lists()
    {
        var markdown = 
            "* Item 1\n" +
            "* Item 2";
        var expected = "<ul><li>Item 1</li><li>Item 2</li></ul>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Fact]
    public void With_a_little_bit_of_everything()
    {
        var markdown = 
            "# Header!\n" +
            "* __Bold Item__\n" +
            "* _Italic Item_";
        var expected = "<h1>Header!</h1><ul><li><strong>Bold Item</strong></li><li><em>Italic Item</em></li></ul>";
        Assert.Equal(expected, Markdown.Parse(markdown));
    }

    [Theory]
    [InlineData("abc\ndef", "<p>abc</p><p>def</p>")]
    [InlineData("#abc", "<p>#abc</p>")]
    [InlineData("##abc", "<p>##abc</p>")]
    [InlineData("###abc", "<p>###abc</p>")]
    [InlineData("####abc", "<p>####abc</p>")]
    [InlineData("#####abc", "<p>#####abc</p>")]
    [InlineData("######abc", "<p>######abc</p>")]
    [InlineData("#######abc", "<p>#######abc</p>")]
    [InlineData("#", "<p>#</p>")]
    [InlineData("#\n#\n", "<p>#</p><p>#</p>")]
    public void HeadingTests(string markdown, string expected)
    {
        Assert.Equal(expected,Markdown.Parse(markdown));
    }
    
    [Theory]
    [InlineData("", "")]
    [InlineData(null, "")]
    [InlineData("\n", "<p></p>")]
    [InlineData("\n\n", "<p></p><p></p>")]
    public void EmptyTextTests(string markdown, string expected)
    {
        Assert.Equal(expected,Markdown.Parse(markdown));
    }
    
    [Theory]
    [InlineData("__abc", "<p>__abc</p>")]
    [InlineData("__abc_", "<p><em>_abc</em></p>")]
    [InlineData("_abc__", "<p><em>abc_</em></p>")]
    [InlineData("__abc\n", "<p>__abc</p>")]
    [InlineData("__abc_\n", "<p><em>_abc</em></p>")]
    [InlineData("_abc__\n", "<p><em>abc_</em></p>")]
    [InlineData("_abc__\n bc", "<p><em>abc_</em></p><p> bc</p>")]
    public void StrongOrItalicTests(string markdown, string expected)
    {
        Assert.Equal(expected,Markdown.Parse(markdown));
    }

    [Theory]
    [InlineData("**", "<p>**</p>")]
    [InlineData("*", "<p>*</p>")]
    [InlineData("*\n*\n", "<p>*</p><p>*</p>")]
    [InlineData("* good list\n*\n", "<ul><li>good list</li></ul><p>*</p>")]
    [InlineData("* good list\n*", "<ul><li>good list</li></ul><p>*</p>")]
    [InlineData("* good list\n", "<ul><li>good list</li></ul>")]
    public void ListTests(string markdown, string expected)
    {
        Assert.Equal(expected,Markdown.Parse(markdown));
    }
}