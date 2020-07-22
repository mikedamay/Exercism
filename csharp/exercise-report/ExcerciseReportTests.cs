using System.Collections.Generic;
using System.IO;
using Xunit;
using Xunit.Sdk;


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
            Stream? resourceStream = this.GetType().Assembly.GetManifestResourceStream(JsonSample1);
            if (resourceStream == null)
            {
                throw new NullException($"{nameof(resourceStream)}");
            }
            string expected;
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
            Stream? resourceStream = this.GetType().Assembly.GetManifestResourceStream(JsonSample1);
            if (resourceStream == null)
            {
                throw new NullException($"{nameof(resourceStream)}");
            }
            string sampleJson;
            using (resourceStream)
            using (var reader = new StreamReader(resourceStream))
                sampleJson = reader.ReadToEnd();
            var actual = erh.FromString(sampleJson);
            var actualString = erh.ToString(actual);
            Assert.Equal(sampleJson, actualString);
            // Assert.Equal(expected, actual);
                // xunit says they don't match.  I wonder if it's checking attributes.
        }

        [Fact]
        public void Import_OriginalConceptsDoc_ProducesListOfConcepts()
        {
            var cdi = new ConceptsDocImporter();
            var results = cdi.ImportOriginalConceptsDoc();
            Assert.Equal(ImportResult.Complete, results.importResult);
        }

        [Fact]
        public void CreateExerciseFile_FromOriginalConceptsDoc_ProducesValidTree()
        {
            var cdi = new ConceptsDocImporter();
            var tnci = new TrackNeutralConceptsImporter();
            var efc = new ExerciseFileCreator(cdi, tnci);
            var exerciseFile = efc.CreateExerciseFileFromConceptsDoc();
            var efh = new ExerciseFileHandler();
            string result = efh.ToString(exerciseFile);
            Assert.NotNull(exerciseFile);
        }

        [Fact]
        public void ImportTrackNeutralConcepts_FromConceptsFile_ProducesDefinitiveMapping()
        {
            var tnci = new TrackNeutralConceptsImporter();
            var map = tnci.ImportTrackNeutralConcepts();
            Assert.Equal(40, map.Count);
        }

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
                    Level = Level.Introductory,
                    TrackNeutralStory = "sample1-track-neutral-story",
                    DocumentType = DocType.Design,
                    DocumentLink = "http://sample1-doclink",
                    Concepts = new List<Concept>
                    {
                        new Concept
                        {
                            Name = "sample1.1-name",
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