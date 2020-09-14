using System;
using System.IO;
using ExerciseValidation;
using ExerciseReport;

namespace ExerciseValidationTests
{
    internal static class Utils
    {
        public static string GetResourceAsString(string resourceName)
        {
            string resourcePath = $"ExerciseValidation.Data.{resourceName}";
            string markdownText = String.Empty;
            Stream? stream = typeof(Utils).Assembly.GetManifestResourceStream(resourcePath);
            if (stream != null)
            {
                using (stream)
                using (var reader = new StreamReader(stream))
                    markdownText = reader.ReadToEnd();
                return markdownText;
            }
            else
            {
                throw new NullReferenceException($"{nameof(stream)} is null - missing resource {resourcePath}");
            }
        }

        public static ExerciseComparer
            GetComparerFromResources(string exercisesResourceName, string designsResourceName)
        {
            return (
                new ExerciseComparer(new ExerciseReader(
                        new ExerciseResourceHandler(exercisesResourceName),
                        new ExerciseJsonParser()),
                    new TrackConfigReader(
                        new TrackConfigResourceHandler(designsResourceName),
                        new TrackConfigJsonParser()))
            );
        }

        /*
        public static (ExerciseMerger ExerciseMerger, TrackConfigResourceHandler TrackConfigResourceHandler)
            GetMergerFromResourcesPlusHandler(string exercisesResourceName, string designsResourceName)
        {
            var erh = new TrackConfigResourceHandler(exercisesResourceName);
            return (
                new ExerciseMerger(new ExerciseReader(
                        erh,
                        new ExerciseJsonParser()),
                    new DesignDocReader(
                        new DesignDocResourceHandler(designsResourceName),
                        new DesignDocParser())),
                erh);
        }
        public static ExerciseMerger
            GetMergerFromResources(string exercisesResourceName, string designsResourceName)
        {
            return (
                new ExerciseMerger(new ExerciseReader(
                        new TrackConfigResourceHandler(exercisesResourceName),
                        new ExerciseJsonParser()),
                    new DesignDocReader(
                        new DesignDocResourceHandler(designsResourceName),
                        new DesignDocParser()))
                );
        }
    */
    }
}