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
            var markdown = Utils.GetResourceAsString(
                Constants.DesignBrokenConceptsResource);
            var results = ddp.ParseDesignDoc(Constants.DesignBrokenConceptsResource,
                markdown).ToList(); 
            Assert.Equal(Result.Errors, results[0].Result);
        }

        [Fact]
        public void ParseDesignDoc_WithConceptsWithoutBullets_ReportsErrors()
        {
            const string resourceName = Constants.DesignConceptsWithoutBulletsResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(Result.Errors, results[0].Result);
            Assert.Contains("no learning objectives", results[0].Error);
        }

        [Fact]
        public void ParseDesignDoc_WithConceptsWithoutQuotes_ReportsErrors()
        {
            const string resourceName = Constants.DesignConceptsWithoutQuotesResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(Result.Errors, results[0].Result);
            Assert.Contains("invalid format", results[0].Error);
        }

        [Fact]
        public void ParseDesignDoc_WithJustConcepts_ReportsNoErrors()
        {
            const string resourceName = Constants.DesignJustConceptsResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(Result.Success, results[0].Result);
        }

        [Fact]
        public void ParseDesignDoc_WithMultipleLearningObjectives_ProducesList()
        {
            const string resourceName = Constants.DesignMultipleObjectivesResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(8, results.Count);
        }

        [Fact]
        public void ParseDesignDoc_WithNoConceptSection_ReportsErrors()
        {
            const string resourceName = Constants.DesignNoConceptSectionResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(Result.Errors, results[0].Result);
            Assert.Contains("no learning objectives", results[0].Error);
        }

        [Fact]
        public void ParseDesignDoc_WithNoConcepts_ReportsErrors()
        {
            const string resourceName = Constants.DesignNoConceptsResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(Result.Errors, results[0].Result);
            Assert.Contains("no learning objectives", results[0].Error);
        }

        [Fact]
        public void ParseDesignDoc_WithNoConceptsTrailing_ReportsErrors()
        {
            const string resourceName = Constants.DesignNoConceptsTrailingResource;
            var ddp = new DesignDocParser();
            var markdown = Utils.GetResourceAsString(resourceName);
            var results = ddp.ParseDesignDoc(resourceName, markdown).ToList(); 
            Assert.Equal(Result.Errors, results[0].Result);
            Assert.Contains("no learning objectives", results[0].Error);
        }
    }
}