using System.Collections.Generic;

namespace ExerciseReport.Tests
{
    internal static class ExerciseTestData
    {
        public static IDictionary<string, ExerciseObjectTree> Exercises { get; } =
            new Dictionary<string, ExerciseObjectTree>
            {
                ["simple"] =
                    new ExerciseObjectTree
                    {
                        Exercises = new List<Exercise>
                        {
                            new Exercise
                            {
                                Slug = "sample1-slug",
                                Level = Level.Introductory,
                                CompletionStatus = CompletionStatus.Complete,
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
                            } // new Exercise
                        } // new List<Exercises>
                    } // new ExerciseObjectTree (simple)
            }; // new Dictionary
    }
}