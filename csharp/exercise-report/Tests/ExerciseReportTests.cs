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
            ejp.FromString(Utils.GetResourceAsString(Constants.ExercisesMissingFieldsResource));
        }
        
        [Fact]
        public void Parse_WellFormedDesignDoc_ProducesConceptLearningObjectives()
        {
            const string SampleDesignDoc = Constants.SampleDesignResource;

            var ddp = new DesignDocParser();
            string markdownText = Utils.GetResourceAsString(SampleDesignDoc);

            var lo = ddp.ParseDesignDoc("embedded-resource.md", markdownText).ToList();
            Assert.NotEmpty(lo);
        }

        [Fact]
        public void ListExercises_OnExercismGit_ProducesFullList()
        {
            const string ExercismRoot = Constants.TestUserRoot;
            var ddc = new DesignDocCollator(new DesignDocFileHandler(ExercismRoot, "csharp"), new DesignDocParser());
            var actual = ddc.GetLearningObjectives(Constants.CSharpTrack);
        }

        [Fact]
        public void Merge_ExercisesAndDesignDocs_ShowsLearningObjectives()
        {
            string ExercismRoot = PathNames.Test.Root;
            const string Track = Constants.CSharpTrack;
            var em = new ExerciseMerger(Track, new ExerciseFileCollator( new ExerciseFileHandler(ExercismRoot, Track),
                    new ExerciseJsonParser())
                , new DesignDocCollator(new DesignDocResourceHandler(), new DesignDocParser()));
            em.Merge();
        }

        [Fact]
        public void Merge_BadJsonSyntaxFile_ReportsFatalError()
        {
            var exerciseResourceHandler = new ExerciseResourceHandler(
                Constants.DesignBrokenConceptsResource);
            var merger = Utils.GetMergerFromResources(
                Constants.DesignBrokenConceptsResource,
                Constants.ExercisesResource,
                exerciseResourceHandler);
            merger.Merge();
        }
        [Fact]
        public void Report_OnExerciseFileTree_ProducesWellFormedReport()
        {
            var rr = new Reporter("https://github.com/mikedamay/v3/tree/csharp/exercise-report");
            var merger = Utils.TestMergerWithResources;
            // var merger = ExerciseMerger.TestMergerWithFileSystem;
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
    }
}