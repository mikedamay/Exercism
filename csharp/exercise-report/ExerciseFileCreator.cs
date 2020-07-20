using System;

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
            (var importResult, var concepts, var errors) = importer.ImportOriginalConceptsDoc();
            if (importResult == ImportResult.Incomplete)
            {
                throw new Exception("Too many errors were encountered importing the ocriginal concepts document");
            }

            foreach (ImportedConcept concept in concepts)
            {
                if (concept.CanonicalConceptName != string.Empty)
                {
                    Exercise exercise = new Exercise
                    {
                        Slug = concept.CanonicalConceptName,
                        DocumentType = concept.DocType == "I" ? DocType.Issue :
                            concept.DocType == "E" ? DocType.Design : DocType.None,
                        DocumentLink = concept.Link
                    };
                }
            }
            return new ExerciseFile();
        }
    }
}