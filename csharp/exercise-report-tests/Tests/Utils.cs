using System;
using System.IO;

namespace ExerciseReport.Tests
{
    internal static class Utils
    {
        public static string GetResourceAsString(string resourceName)
        {
            string resourcePath = $"ExerciseReport.Tests.Data.{resourceName}";
            string markdownText = String.Empty;
            Stream? stream = typeof(ExerciseReportTests).Assembly.GetManifestResourceStream(resourcePath);
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

        public static (ExerciseMerger ExerciseMerger, ExerciseResourceHandler ExerciseResourceHandler)
            GetMergerFromResourcesPlusHandler(string exercisesResourceName, string designsResourceName)
        {
            var erh = new ExerciseResourceHandler(exercisesResourceName);
            return (
                new ExerciseMerger(new ExerciseFileCollator(
                        erh,
                        new ExerciseJsonParser()),
                    new DesignDocCollator(
                        new DesignDocResourceHandler(designsResourceName),
                        new DesignDocParser())),
                erh);
        }

        public static ExerciseMerger TestMergerWithResources { get; } =
            GetMergerFromResourcesPlusHandler(Constants.ExercisesResource,
                Constants.ManyDesignsResource).ExerciseMerger;
    }
}