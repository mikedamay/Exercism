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

            var lo = ddp.ParseDesignDoc(markdownText).ToList();
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
    }
}