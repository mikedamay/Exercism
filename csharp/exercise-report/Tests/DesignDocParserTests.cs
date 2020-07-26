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
            var results = ddp.ParseDesignDoc(Constants.DesignBrokenConceptsResource,
                markdown).ToList(); 
            Assert.False(results[0].success);
        }

        [Fact]
        public void ParseDesignDoc_WithConceptsWithoutBullets_ReportsErrors()
        {
            const string resourceName = Constants.DesignConceptsWithoutBulletsResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.False(results[0].success);
            Assert.Contains("no learning objectives", results[0].error);
        }

        [Fact]
        public void ParseDesignDoc_WithConceptsWithoutQuotes_ReportsErrors()
        {
            const string resourceName = Constants.DesignConceptsWithoutQuotesResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.False(results[0].success);
            Assert.Contains("invalid format", results[0].error);
        }

        [Fact]
        public void ParseDesignDoc_WithJustConcepts_ReportsNoErrors()
        {
            const string resourceName = Constants.DesignJustConceptsResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.True(results[0].success);
        }

        [Fact]
        public void ParseDesignDoc_WithMultipleHashes_ReportsNoErrors()
        {
            const string resourceName = Constants.DesignMultipleHashesResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.True(results[0].success);
        }

        [Fact]
        public void ParseDesignDoc_WithNoConceptSection_ReportsErrors()
        {
            const string resourceName = Constants.DesignNoConceptSectionResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.False(results[0].success);
            Assert.Contains("no learning objectives", results[0].error);
        }

        [Fact]
        public void ParseDesignDoc_WithNoConcepts_ReportsErrors()
        {
            const string resourceName = Constants.DesignNoConceptsResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.False(results[0].success);
            Assert.Contains("no learning objectives", results[0].error);
        }

        [Fact]
        public void ParseDesignDoc_WithNoConceptsTrailing_ReportsErrors()
        {
            const string resourceName = Constants.DesignNoConceptsTrailingResource;
            var ddp = new DesignDocParser();
            var markdown = ExerciseReportTests.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.False(results[0].success);
            Assert.Contains("no learning objectives", results[0].error);
        }
    }
}