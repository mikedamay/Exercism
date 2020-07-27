using System;
using System.IO;

namespace ExerciseReport.Tests
{
    internal class Utils
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

        public static ExerciseMerger TestMergerWithResources { get; } =
            new ExerciseMerger(Constants.CSharpTrack, 
                new ExerciseFileCollator(
                    new ExerciseResourceHandler(), 
                    new ExerciseJsonParser()),
                new DesignDocCollator(Constants.TestUserRoot,
                    new DesignDocParser(),
                    new DesignDocResourceHandler()));

        public static ExerciseMerger TestMergerWithFileSystem { get; } =
            new ExerciseMerger(Constants.CSharpTrack,
                new ExerciseFileCollator(
                    new ExerciseFileHandler(PathNames.Test.Root, Constants.CSharpTrack), 
                    new ExerciseJsonParser()),
                new DesignDocCollator(PathNames.Test.Root, new DesignDocParser(),
                    new DesignDocFileHandler(PathNames.Test.Root,
                        Constants.CSharpTrack)));
    }
}