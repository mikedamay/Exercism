using System;
using System.Collections.Generic;

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
            
            (var importResult, var concepts, _) = importer.ImportOriginalConceptsDoc();
            if (importResult == ImportResult.Incomplete)
            {
                throw new Exception("Too many errors were encountered importing the original concepts document");
            }

            foreach (ImportedConcept importedConcept in concepts)
            {
                if (importedConcept.Rank <= 0 || importedConcept.Rank >= 100 && importedConcept.Rank != 800)
                {
                    continue;    // section headings, unclassified concepts, report headings
                }
                if (importedConcept.Rank >= 0 && importedConcept.Rank <= 100)
                {
                    OriginalConcept originalConcept = new OriginalConcept
                    {
                        Name = importedConcept.OriginalConceptName,
                        LineNumber = importedConcept.ConceptsDocLineNum
                    };
                    Concept concept = new Concept
                    {
                        Name = importedConcept.CanonicalConceptName,
                        OriginalConcepts = new List<OriginalConcept>{originalConcept}
                    };
                    Exercise exercise = new Exercise
                    {
                        Slug = importedConcept.CanonicalConceptName,
                        DocumentType = importedConcept.DocType == "I" ? DocType.Issue :
                            importedConcept.DocType == "E" ? DocType.Design : DocType.None,
                        DocumentLink = importedConcept.Link,
                        Concepts = new List<Concept>{concept}
                    };
                    conceptMap[concept.Name] = concept;
                    exerciseMap[exercise.Slug] = exercise;
                }
                else // importedConcept.Rank == 800
                {
                    OriginalConcept originalConcept = new OriginalConcept
                    {
                        Name = importedConcept.OriginalConceptName,
                        LineNumber = importedConcept.ConceptsDocLineNum
                    };
                    Concept concept;
                    if (conceptMap.ContainsKey(importedConcept.CanonicalConceptName))
                    {
                        concept = conceptMap[importedConcept.CanonicalConceptName];
                    }
                    else
                    {
                        concept = new Concept();
                        Exercise exercise = exerciseMap[importedConcept.CanonicalConceptName];
                        exercise.Concepts.Add(concept);
                    }

                    concept.Name = importedConcept.CanonicalConceptName;
                    concept.OriginalConcepts.Add(originalConcept);
                }
            }
            return exerciseFile;
        }
    }
}