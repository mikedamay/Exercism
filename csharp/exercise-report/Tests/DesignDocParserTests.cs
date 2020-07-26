using System.Linq;
using Xunit;

namespace ExerciseReport.Tests
{
    public class DesignDocParserTests
    {
        [Fact]
        public void ParseDesignDoc_WithBrokenConcepts_ReportsErrors()
        {
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(
                Constants.DesignBrokenConceptsResource);
            var results = ddp.ParseDesignDoc("embedded-resource.md", markdown).ToList(); 
            Assert.False(results[0].success);
        }
    }
}