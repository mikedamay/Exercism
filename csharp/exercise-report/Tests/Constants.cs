using System;

namespace ExerciseReport.Tests
{
    internal static class Constants
    {
        public const string ManyDesignsResource = "many_designs.md";
        public const string SampleDesignResource = "sample_design.md";

        public const string ExercisesBadDocTypeResource = "exercises_bad_doctype.json";
        public const string ExercisesBadLevelResource = "exercises_bad_level.json";
        public const string ExercisesEmptyResource = "exercises_empty.json";
        public const string ExercisesMissingLevelResource = "exercises_missing_level.json";
        public const string ExercisesMissingConceptsResource = "exercises_missing_concepts.json";
        public const string ExercisesMissingConceptNameResource = "exercises_missing_concept_name.json";
        public const string ExercisesMissingDocumentLinkResource = "exercises_missing_document_link.json";
        public const string ExercisesMissingDocumentTypeResource = "exercises_missing_document_type.json";
        public const string ExercisesMissingFieldsResource = "exercises_missing_fields.json";
        public const string ExercisesMissingSlugResource = "exercises_missing_slug.json";
        public const string ExercisesResource = "exercises.json";
        public const string ExercisesSlightlyWrongResource = "exercises_slightly_wrong.json";
        public const string ExercisesWrongStructureResource = "exercises_bad_format.json";

        public static readonly string DesignDocSeparator = Environment.NewLine + "separator-1729" + Environment.NewLine;

        public const string TestUserRoot = "/Users/mikedamay/projects/exercism/v3";

        public const string CSharpTrack = "csharp";

        public const int MaxErrors = 20;
    }
}