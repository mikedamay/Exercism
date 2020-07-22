using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport
{
    internal class ExerciseFileCreator
    {
        private ConceptsDocImporter importer;
        private TrackNeutralConceptsImporter tncImporter;
        private const string LINK_NAME = "linkName"; 
        private Regex regex = new Regex(@$"\[.+\]\[(?<{LINK_NAME}>.+)\].*");

        public ExerciseFileCreator(ConceptsDocImporter importer, TrackNeutralConceptsImporter tncImporter)
        {
            this.importer = importer;
            this.tncImporter = tncImporter;
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

            (var importResult, var importedConcepts, _) = importer.ImportOriginalConceptsDoc();
            if (importResult == ImportResult.Incomplete)
            {
                throw new Exception("Too many errors were encountered importing the original concepts document");
            }

            exerciseFile.Exercises = importedConcepts.Where(ic => ic.Rank > 0 && ic.Rank < 100)
                .Select(ic =>
                    new Exercise
                    {
                        Slug = ic.CanonicalConceptName,
                        Level = ic.Section switch
                        {
                            "A" => Level.Introductory,
                            "B" => Level.Essential,
                            "C" => Level.Advanced,
                            _ => Level.None                            
                        },
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

            importedConcepts.Where(ic => ic.Rank == 800)
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
                    : unallocatedConceptsExercise.Concepts.AddNew(p.c)).ToNull();
            var trackNeutralConceptMap = tncImporter.ImportTrackNeutralConcepts();
            // The next statement identifies any original concept that is linked to
            // a track-neutral concept and puts the url into the concept record.
            //
            // if a concept has more than one original concept which contains a track neutral link
            // then the last one processed will be used
            exerciseFile.Exercises
                .SelectMany(ex => ex.Concepts)
                .SelectMany(con => con.OriginalConcepts, (con, ocon) => (con, ocon))
                .Where(cons => IsTrackNeutralLink(cons.ocon.Name))
                .Select(cons => (cons.con, GetTrackNeutralLinkName(cons.ocon.Name)))
                .Select(conss => conss.con.TrackNeutralConcept
                    = GetTrackNeutralLinkUrl(conss.Item2, trackNeutralConceptMap)).ToNull();

            return exerciseFile;
        }

        private string GetTrackNeutralLinkUrl(string tncName, IDictionary<string,string> trackNeutralConceptMap)
        {
            return trackNeutralConceptMap.ContainsKey(tncName) 
                ? trackNeutralConceptMap[tncName].Trim()
                : tncName.Trim();
        }

        private bool IsTrackNeutralLink(string originalConceptName)
        {
            var match = regex.Match(originalConceptName);
            if (!match.Success)
            {
                return false;
            }
            return match.Groups.ContainsKey(LINK_NAME);
        }

        private string GetTrackNeutralLinkName(string originalConceptName)
        {
            var match  = regex.Match(originalConceptName);
            MyDebug.Assert(() => match.Groups.ContainsKey(LINK_NAME)
                , "This should part of a filtered LINQ pipeline");
            return match.Groups[LINK_NAME].Value;
        }
    }

    public static class Extensions
    {
        public static bool AddNew<T>(this IList<T> list, T item)
        {
            list.Add(item);
            return true;
        }

        public static void ToNull<T>(this IEnumerable<T> source)
        {
            foreach (var s in source) ;
        }
    }
}