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
            string markdownText;
            Stream? stream = this.GetType().Assembly.GetManifestResourceStream(SampleDesignDoc);
            if (stream == null)
            {
                Assert.False(true);
            }
            using (stream)
            using (var reader = new StreamReader(stream))
                markdownText = reader.ReadToEnd();
            var lo = ddp.ParseDesignDoc(markdownText).ToList();
            Assert.NotEmpty(lo);
        }

        [Fact]
        public void ListExercises_OnExercismGit_ProducesFullList()
        {
            var ddc = new DesignDocCollator("/Users/mikedamay/projects/exercism/v3");
            var actual = ddc.GetLearningObjectives();
        }
    }
}