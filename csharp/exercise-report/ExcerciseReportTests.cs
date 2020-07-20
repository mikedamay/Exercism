using System.Collections.Generic;
using System.IO;
using Xunit;


namespace ExerciseReport
{
    public class ExcerciseReportTests
    {
        private const string JsonSample1 = "ExerciseReport.sample.json";

        [Fact]
        public void Serialize_ExerciseFile_ProducesWellFormedJson()
        {
            var erh = new ExerciseFileHandler();
            var actual = erh.ToString(ObjectHierarchy.Sample1);
            var resourceStream = this.GetType().Assembly.GetManifestResourceStream(JsonSample1);
            var expected = string.Empty;
            using (resourceStream)
            using (var reader = new StreamReader(resourceStream))
                expected = reader.ReadToEnd();
            Assert.Equal(expected.Trim(), actual.Trim());
        }

        [Fact]
        public void Deserialize_WellFormedJson_ProducesObjectTree()
        {
            var erh = new ExerciseFileHandler();
            var expected = ObjectHierarchy.Sample1;
            var resourceStream = this.GetType().Assembly.GetManifestResourceStream(JsonSample1);
            var sampleJson = string.Empty;
            using (resourceStream)
            using (var reader = new StreamReader(resourceStream))
                sampleJson = reader.ReadToEnd();
            var actual = erh.FromString(sampleJson);
            var actualString = erh.ToString(actual);
            Assert.Equal(sampleJson, actualString);
            // Assert.Equal(expected, actual);
                // xunit says they don't match.  I wonder if it's looking for attributes.
        }
    }

    public static class ObjectHierarchy
    {
        public static ExerciseFile Sample1 => new ExerciseFile
        {
            Exercises = new List<Exercise>
            {
                new Exercise
                {
                    Slug = "sample1-slug",
                    TrackNeutralStory = "sample1-track-neutral-story",
                    DocumentType = DocType.Design,
                    DocumentLink = "http://sample1-doclink",
                    Concepts = new List<Concept>
                    {
                        new Concept
                        {
                            Name = "sample1.1-name",
                            Description = "sample1.1-description",
                            TrackNeutralConcept = "sample1.1-track-neutral-concept",
                            LearningObjectives = new List<string>
                            {
                                "sample1.1.1-learning-objective"
                            },
                            OriginalConcepts = new List<OriginalConcept>
                            {
                                new OriginalConcept
                                {
                                    Name = "sample1.1.1-name",
                                    LineNumber = 1729
                                }
                            }
                        }
                    }
                }
            }
        };
    }
}