using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;

namespace ExerciseReport
{
    internal class ExerciseFileCreator
    {
        private ConceptsDocImporter importer;

        public ExerciseFileCreator(ConceptsDocImporter importer)
        {
            this.importer = importer;
        }

        public ExerciseFile CreateExerciseFileFromConceptsDoc()
        {
            var conceptMap = new Dictionary<string, Concept>();
            var exerciseMap = new Dictionary<string, Exercise>();
            var exerciseFile = new ExerciseFile();
            var unallocatedConceptsExercise = new Exercise
            {
                Slug = "unallocated-concepts"
            };

            (var importResult, var concepts, _) = importer.ImportOriginalConceptsDoc();
            if (importResult == ImportResult.Incomplete)
            {
                throw new Exception("Too many errors were encountered importing the original concepts document");
            }

            exerciseFile.Exercises = concepts.Where(ic => ic.Rank > 0 && ic.Rank < 100)
                .Select(ic =>
                    new Exercise
                    {
                        Slug = ic.CanonicalConceptName,
                        DocumentType = ic.DocType == "I" ? DocType.Issue :
                            ic.DocType == "E" ? DocType.Design : DocType.None,
                        DocumentLink = ic.Link,
                        Concepts = new List<Concept>
                        {
                            new Concept
                            {
                                Name = ic.CanonicalConceptName,
                                OriginalConcepts = new List<OriginalConcept>
                                {
                                    new OriginalConcept
                                    {
                                        Name = ic.OriginalConceptName,
                                        LineNumber = ic.ConceptsDocLineNum
                                    }
                                }
                            }
                        }
                    }).ToList();
            exerciseFile.Exercises.Add(unallocatedConceptsExercise);
            exerciseMap = exerciseFile.Exercises.ToDictionary((ex) => ex.Slug, (ex) => ex);

            concepts.Where(ic => ic.Rank == 800)
                .Select<ImportedConcept, (ImportedConcept ic, Concept c)>(ic => (ic, new Concept
                {
                    Name = ic.CanonicalConceptName,
                    OriginalConcepts = new List<OriginalConcept>
                    {
                        new OriginalConcept
                        {
                            Name = ic.OriginalConceptName,
                            LineNumber = ic.ConceptsDocLineNum
                        }
                    }
                })).Select(p => exerciseMap.ContainsKey(p.ic.FurtherInfo)
                    ? exerciseMap[p.ic.FurtherInfo].Concepts.AddNew(p.c)
                    : unallocatedConceptsExercise.Concepts.AddNew(p.c)).ToList();
            return exerciseFile;
        }
    }

    public static class Extensions
    {
        public static bool AddNew<T>(this IList<T> list, T item)
        {
            list.Add(item);
            return true;
        }
    }
}