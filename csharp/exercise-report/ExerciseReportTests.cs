using System.IO;
using System.Linq;
using Xunit;

namespace ExerciseReport
{
    public class ExerciseReportTests
    {
        [Fact]
        public void Parse_WellFormedDesignDoc_ProducesConceptLearningObjectives()
        {
            const string SampleDesignDoc = "ExerciseReport.sample_design.md";
        
            var ddp = new DesignDocParser();
            string markdownText = string.Empty;
            Stream? stream = this.GetType().Assembly.GetManifestResourceStream(SampleDesignDoc);
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    markdownText = reader.ReadToEnd();
            }
            else
            {
                Assert.False(true);
            }

            var lo = ddp.ParseDesignDoc(markdownText, "test-track").ToList();
            Assert.NotEmpty(lo);
        }

        [Fact]
        public void ListExercises_OnExercismGit_ProducesFullList()
        {
            const string ExercismRoot = "/Users/mikedamay/projects/exercism/v3";
            var ddc = new DesignDocCollator(ExercismRoot, new DesignDocParser()
            , new DesignDocFileHandler(ExercismRoot));
            var actual = ddc.GetLearningObjectives("csharp");
        }

        [Fact]
        public void Merge_ExercisesAndDesignDocs_ShowsLearningObjectives()
        {
            const string ExercismRoot = "/Users/mikedamay/projects/exercism/v3";
            const string Track = "csharp";
            var em = new ExerciseMerger(Track, new ExerciseFileHandler(ExercismRoot,Track,
                new ExerciseFileJsonHandler())
                , new DesignDocCollator(ExercismRoot, new DesignDocParser()
                    , new DesignDocFileHandler(ExercismRoot) ));
            em.Merge();
        }

        [Fact]
        public void Report_OnExerciseFileTree_ProducesWellFormedReport()
        {
            var rr = new Reporter("https://github.com/mikedamay/v3/tree/csharp/interfaces");
            var merger = ExerciseMerger.TestCSharpMerger;
            var exerciseFile = merger.MergeLearningObjectives();
            var output = rr.CreateReport(exerciseFile);
            Assert.NotEmpty(output);
        }
    }
}