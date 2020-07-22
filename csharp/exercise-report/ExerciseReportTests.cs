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
            using (stream)
            using (var reader = new StreamReader(stream))
                markdownText = reader.ReadToEnd();
            var lo = ddp.ParseDesignDoc(markdownText);
            Assert.True(lo.GetList("string-formatting").First() != string.Empty);
        }
        
    }
}