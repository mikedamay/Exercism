using System;
using System.Collections.Generic;
using System.IO;

namespace ExerciseReport
{
    internal enum ImportResult
    {
        Complete,
        Incomplete
    }
    internal class ConceptsDocImporter
    {
        private const int MAX_ERRORS = 60;
        private const string OriginalConceptsDoc = "ExerciseReport.original_concepts_doc.csv";

        private enum SplitResult
        {
            Concept,
            Error
        }

        public (ImportResult importResult, IList<ImportedConcept> importedConcepts, IList<ImportError> errors) 
            ImportOriginalConceptsDoc()
        {
            var originalConceptsDoc = this.GetType().Assembly.GetManifestResourceStream(OriginalConceptsDoc);
            if (originalConceptsDoc == null)
            {
                throw new InvalidOperationException(
                    $"ImportOriginalConceptsDoc: Failed to find embedded resource {OriginalConceptsDoc}");
            }

            string originalConcepts = string.Empty;
            try
            {
                using var doc = originalConceptsDoc;
                using var reader = new StreamReader(doc);
                originalConcepts = reader.ReadToEnd();
            }
            catch (Exception ex)
            {
                throw new Exception("ImportOriginalConceptsDoc: weird resource read failure", ex);
            }

            return ImportOriginalConceptsDoc(originalConcepts);
        }

        public (ImportResult importResult, IList<ImportedConcept> importedConcepts, IList<ImportError> errors)
            ImportOriginalConceptsDoc(string importedConceptsCsv)
        {
            var importedConcepts = new List<ImportedConcept>();
            var errors = new List<ImportError>();
            int lineNum = 0;

            foreach (var importedConcept in importedConceptsCsv.Split("\n"))
            {
                var results = SplitLine(importedConcept, lineNum);

                switch (results)
                {
                    case (SplitResult.Concept, ImportedConcept ic, _):
                        importedConcepts.Add(ic);
                        break;
                    case (SplitResult.Error, _, ImportError ie):
                        errors.Add(ie);
                        if (errors.Count > MAX_ERRORS)
                        {
                            goto after_loop;
                        }

                        break;
                }
                lineNum++;
            }
            after_loop:

            return (errors.Count > MAX_ERRORS ? ImportResult.Incomplete : ImportResult.Complete, importedConcepts,
                errors);
        }

        private (SplitResult splitResult, ImportedConcept? importedConcept, ImportError? error) 
            SplitLine(string importedLineCsv, int lineNum)
        {
            const int ROW_NUM = 0;
            const int ORIGINAL_LINE_NUM = 1;
            const int ORIGINAL_CONCEPT_NAME = 2;
            const int CANONICAL_CONCEPT_NAME = 3;
            const int RANK = 4;
            const int SECTION = 5;
            const int DOC_TYPE = 6;
            const int LINK = 7;
            const int FURTHER_INFO = 8;
            const int NUM_PARTS = FURTHER_INFO + 1;

            string[] parts = importedLineCsv.Split(",");
            if (parts.Length < NUM_PARTS)
            {
                var error = new ImportError(lineNum,
                    $"Only {parts.Length} values in the line were found, {NUM_PARTS} were expected", importedLineCsv);
                return (SplitResult.Error, null, error);
            }

            try
            {
                ImportedConcept concept = new ImportedConcept
                {
                    SourceRowNum = Int32.Parse(parts[ROW_NUM]),
                    ConceptsDocLineNum = Int32.Parse(parts[ORIGINAL_LINE_NUM]),
                    OriginalConceptName = parts[ORIGINAL_CONCEPT_NAME],
                    CanonicalConceptName = parts[CANONICAL_CONCEPT_NAME],
                    Rank = Int32.Parse(parts[RANK]),
                    Section = parts[SECTION],
                    DocType = parts[DOC_TYPE],
                    Link = parts[LINK],
                    FurtherInfo = parts[FURTHER_INFO]
                };
                return (SplitResult.Concept, concept, null);
            }
            catch (FormatException fe)
            {
                var _ = fe;
                return (SplitResult.Error, null, new ImportError(lineNum, "Numeric conversion failure", importedLineCsv));
            }
            catch (Exception e)
            {
                var _ = e;
                return (SplitResult.Error, null, new ImportError(lineNum, "Unspecified failure", importedLineCsv));
            }
        }
    }

    internal class ImportedConcept
    {
        public int SourceRowNum { get; set; }
        public int ConceptsDocLineNum { get; set; }
        public string OriginalConceptName { get; set; } = string.Empty;
        public string CanonicalConceptName { get; set; } = string.Empty;
        public int Rank { get; set; }
        public string Section { get; set; } = string.Empty;
        public string DocType { get; set; } = string.Empty;
        public string Link { get; set; } = string.Empty;
        public string FurtherInfo { get; set; } = string.Empty;

        public static ImportedConcept NullConcept { get; } = new ImportedConcept();
    }

    internal class ImportError
    {
        public int LineNum { get; }
        public string Message { get; }
        public string CsvLine { get; }
        public ImportError(int lineNum, string message, string csvLine)
        {
            LineNum = lineNum;
            Message = message;
            CsvLine = csvLine;
        }
    }
}