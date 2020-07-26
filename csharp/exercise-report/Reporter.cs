using System;
using System.IO;
using System.Linq;
using System.Text;

namespace ExerciseReport
{
    internal class Reporter
    {
        private string root;

        public Reporter(string root)
        {
            this.root = root;
        }

        public string CreateReport(ExerciseObjectTree exerciseObjectTree)
        {
            var concepts = CreateConceptPart(exerciseObjectTree);
            var conceptDefiniitons = CreateLearningObjectivesPart(exerciseObjectTree);
            var linkReferences = CreateLinkReferences(exerciseObjectTree);
            return concepts + conceptDefiniitons + linkReferences;
        }

        private string CreateConceptPart(ExerciseObjectTree exerciseObjectTree)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("# C&#35; reference");
            sb.AppendLine();
            sb.AppendLine("## Concepts");
            sb.AppendLine();
            sb.AppendLine(
                "The C# concept exercises are based on concepts. The list below contains the concepts that have been identified for the C# language.");
            sb.AppendLine();
            sb.AppendLine(
                @"_(Please do not modify this document. it is automatically generated. All text except the concept learning objectives is sourced
                            from [exercises.json](./exercises.json) which should be updated manually when a concept is added or an issue or new design is created
                            and learning objectives are scraped from the concept definition text in each exercise's design.md document)_.");
            sb.AppendLine();
            sb.AppendLine("### Introductory Concepts");
            sb.AppendLine(); 
            GetCncepts(sb, exerciseObjectTree, Level.Introductory);
            sb.AppendLine();
            sb.AppendLine("### Essential Concepts");
            sb.AppendLine(); 
            GetCncepts(sb, exerciseObjectTree, Level.Essential);
            sb.AppendLine();
            sb.AppendLine("### Advanced Concepts");
            sb.AppendLine(); 
            GetCncepts(sb, exerciseObjectTree, Level.Advanced);
            return sb.ToString();
        }

        private string CreateLearningObjectivesPart(ExerciseObjectTree exerciseObjectTree)
        {
            var conceptList = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts)
                .Where(c => c.LearningObjectives.Count > 0)
                .OrderBy(c => c.Name)
                .Select(c => new {name = "`" + c.Name + "`", learningObjectives = string.Join(';', c.LearningObjectives)})
                .ToList();
            int longestConceptName = conceptList.Max(c => c.name.Length);
            int longestLearningObjectives = conceptList.Max(c => c.learningObjectives.Length);
            StringBuilder sb = new StringBuilder();
            var format = "| {0,-" + longestConceptName + "} | {1,-" + longestLearningObjectives + "} |" +Environment.NewLine;
            sb.AppendLine();
            sb.AppendLine("## Learning Objectives");
            sb.AppendLine();
            sb.AppendFormat(format, "Concept", "Learning Objectives");
            sb.AppendFormat(format, new String('-', longestConceptName), new String('-', longestLearningObjectives));

            foreach (var text in conceptList)
            {
                sb.AppendFormat(format, text.name, text.learningObjectives);
            }
            return sb.ToString();
        }

        private string CreateLinkReferences(ExerciseObjectTree exerciseObjectTree)
        {
            StringBuilder sb = new StringBuilder();
            var issuesOrDesigns = exerciseObjectTree.Exercises
                .Where(ex => ex.DocumentType != DocumentType.None)
                .OrderBy(ex => ex.Slug)
                .Select(ex => $"[{(ex.DocumentType == DocumentType.Issue ? "issue-" : "design-") + ex.Slug}]: {ex.DocumentLink}");
 
            sb.AppendLine();
            foreach (string issueOrDesign in issuesOrDesigns)
            {
                sb.AppendLine(issueOrDesign);
            }

            var trackNeutralConcepts = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts)
                .Where(c => !string.IsNullOrWhiteSpace(c.TrackNeutralConcept))
                .OrderBy(c => c.Name)
                .Select(c => $"[tnc-{c.Name}]: {Path.Combine(root,c.TrackNeutralConcept)}");
            foreach (string trackNeutralConcept in trackNeutralConcepts)
            {
                sb.AppendLine(trackNeutralConcept);
            }
            return sb.ToString();
        }

        private void GetCncepts(StringBuilder sb, ExerciseObjectTree exerciseObjectTree, Level level)
        {
            var outputs = exerciseObjectTree.Exercises
                .SelectMany(ex => ex.Concepts, (ex,
                    c) => (ex, c))
                .Where(p => p.Item1.Level == level)
                .OrderBy(p => p.Item2.Name)
                .Select(p => FormatOutput(p.Item1, p.Item2));
            foreach (string output in outputs)
            {
                sb.AppendLine(output);
            }
        }

        private string FormatOutput(Exercise exercise, Concept concept)
        {
            var link = (exercise.DocumentType, concept.TrackNeutralConcept) switch
            {
                (DocumentType.Issue, "") => $" - [Issue][issue-{exercise.Slug}]",
                (DocumentType.Design, "") => $" - [Design][design-{exercise.Slug}]",
                (DocumentType.Issue, _) => $" - [Issue][issue-{exercise.Slug}], [background][tnc-{concept.Name}]",
                (DocumentType.Design, _) => $" - [Design][design-{exercise.Slug}], [background][tnc-{concept.Name}]",
                (DocumentType.None, "") => string.Empty,
                _ => $" - [background][tnc-{concept.Name}]"
            };
            return $"- {concept.Name} _({exercise.Slug})_{link}";
        }
    }
}