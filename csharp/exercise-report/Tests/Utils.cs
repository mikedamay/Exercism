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

        public static ExerciseMerger GetMergerFromResources(
            string exercisesResourceName, string designsResourceName,
            IExerciseFileHandler? exerciseFileHandler = null)
        {
            return 
                new ExerciseMerger(Constants.CSharpTrack, 
                    new ExerciseFileCollator(
                        exerciseFileHandler ?? new ExerciseResourceHandler(exercisesResourceName), 
                        new ExerciseJsonParser()),
                    new DesignDocCollator(
                        new DesignDocResourceHandler(designsResourceName),
                        new DesignDocParser()));
           
        }

        public static ExerciseMerger TestMergerWithResources { get; } =
            GetMergerFromResources(Constants.ExercisesResource,
                Constants.ManyDesignsResource);

        public static ExerciseMerger TestMergerWithFileSystem { get; } =
            new ExerciseMerger(Constants.CSharpTrack,
                new ExerciseFileCollator(
                    new ExerciseFileHandler(PathNames.Test.Root, Constants.CSharpTrack), 
                    new ExerciseJsonParser()),
                new DesignDocCollator(
                    new DesignDocFileHandler(PathNames.Test.Root, Constants.CSharpTrack),
                    new DesignDocParser()));
    }
}