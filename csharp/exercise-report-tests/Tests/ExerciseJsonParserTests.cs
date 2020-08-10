using Xunit;

namespace ExerciseReport.Tests
{
    public class ExerciseJsonParserTests
    {
        [Fact]
        public void ParseExerciseJson_WithInvalidDocType_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesBadDocTypeResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.Contains(nameof(Exercise.CompletionStatus), outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithInvalidLevel_ReportFatalsError()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesBadLevelResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.Contains(nameof(Exercise.Level), outputs.Errors[0].Message);
        }
        
        [Fact]
        public void ParseExerciseJson_WithWrongStructure_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesWrongStructureResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.Contains(nameof(ExerciseObjectTree), outputs.Errors[0].Message);
        }
        
        [Fact]
        public void ParseExerciseJson_WithNonJsonSyntax_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ManyDesignsResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.Contains("'#' is an invalid start", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithSlightlyWrongStructure_ReportsFatalError()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesSlightlyWrongResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.Contains("Json parser failed", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingFields_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingFieldsResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Equal(5, outputs.Errors.Count);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingSlug_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingSlugResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("slug", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingLevel_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingLevelResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("level", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingDocType_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingDocumentTypeResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("completion-status", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingDocLink_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingDocumentLinkResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("document-link", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingConcepts_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingConceptsResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("concepts", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingConceptName_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMissingConceptNameResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("concept.name", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_ForEmptyString_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesEmptyResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.FatalError, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("does not contain any JSON tokens", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithOtioseDocLink_ReportsErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesOtioseDocumentLinkResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Errors, outputs.Result);
            Assert.Single(outputs.Errors);
            Assert.Contains("document-link: present", outputs.Errors[0].Message);
        }

        [Fact]
        public void ParseExerciseJson_WithMissingInessentialFields_ReportsNoErrors()
        {
            var ejp = new ExerciseJsonParser();
            var json = Utils.GetResourceAsString(
                Constants.ExercisesMinimalValidResource);
            var outputs = ejp.FromString(json); 
            Assert.Equal(Result.Success, outputs.Result);
        }
    }
}