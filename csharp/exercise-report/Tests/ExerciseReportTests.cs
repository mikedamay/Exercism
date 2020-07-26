using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Xunit;
using System.Text.Json;

namespace ExerciseReport.Tests
{
    public class ExerciseReportTests
    {
        [Fact]
        public void Parse_ExerciseFileMissingFields_ThrowsException()
        {
            var ejp = new ExerciseJsonParser();
            ejp.FromString(GetResourceAsString(Constants.ExercisesMissingFieldsResource));
        }
        
        [Fact]
        public void Parse_WellFormedDesignDoc_ProducesConceptLearningObjectives()
        {
            const string SampleDesignDoc = Constants.SampleDesignResource;

            var ddp = new DesignDocParser();
            string markdownText = GetResourceAsString(SampleDesignDoc);

            var lo = ddp.ParseDesignDoc(markdownText, Constants.CSharpTrack).ToList();
            Assert.NotEmpty(lo);
        }

        [Fact]
        public void ListExercises_OnExercismGit_ProducesFullList()
        {
            const string ExercismRoot = Constants.TestUserRoot;
            var ddc = new DesignDocCollator(ExercismRoot, new DesignDocParser()
                , new DesignDocFileHandler(ExercismRoot, "csharp"));
            var actual = ddc.GetLearningObjectives(Constants.CSharpTrack);
        }

        [Fact]
        public void Merge_ExercisesAndDesignDocs_ShowsLearningObjectives()
        {
            string ExercismRoot = PathNames.Test.Root;
            const string Track = Constants.CSharpTrack;
            var em = new ExerciseMerger(Track, new ExerciseFileCollator( new ExerciseFileHandler(ExercismRoot, Track),
                    new ExerciseJsonParser())
                , new DesignDocCollator(ExercismRoot, new DesignDocParser()
                    , new DesignDocResoourceHandler()));
            em.Merge();
        }

        [Fact]
        public void Report_OnExerciseFileTree_ProducesWellFormedReport()
        {
            var rr = new Reporter("https://github.com/mikedamay/v3/tree/csharp/exercise-report");
            var merger = ExerciseMerger.TestCSharpMerger;
            var exerciseFile = merger.MergeLearningObjectives();
            var output = rr.CreateReport(exerciseFile.exerciseObjectTree);
            Assert.NotEmpty(output);
        }

        [Fact]
        public void MakeDesignTestData() 
        {
            string Separator = Constants.DesignDocSeparator;
            var ddfh = new DesignDocFileHandler(PathNames.Test.Root, "csharp");
            var designs = ddfh.GetExerciseDesignsForTrack().ToList();
            var designResource = string.Join(Separator, designs);
            var output = Regex.Split(designResource, Separator);
        }

        public static string GetResourceAsString(string resourceName)
        {
            string resourcePath = $"ExerciseReport.Tests.Data.{resourceName}";
            string markdownText = string.Empty;
            Stream? stream = typeof(ExerciseReportTests).Assembly.GetManifestResourceStream(resourcePath);
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    markdownText = reader.ReadToEnd();
                return markdownText;
            }
            else
            {
                throw new NullReferenceException($"{nameof(stream)} is null - missing resource {resourcePath}");
            }
        }
    }
}