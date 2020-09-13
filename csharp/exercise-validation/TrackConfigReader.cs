using System;
using System.Collections.Generic;
using ExerciseReport;

namespace ExerciseValidation
{
    public class TrackConfigReader
    {
        public static TrackConfigReader CSharpTrackConfigReader { get; } =
            new TrackConfigReader(
                new TrackConfigFileHandler(PathNames.Default.Root, Constants.CSharpTrack),
                new TrackConfigJsonParser());

        private readonly ITrackConfigFileHandler trackConfigFileHandler;
        private readonly TrackConfigJsonParser trackConfigJsonParser;

        public TrackConfigReader(ITrackConfigFileHandler trackConfigFileHandler,
            TrackConfigJsonParser trackConfigJsonParser)
        {
            this.trackConfigFileHandler = trackConfigFileHandler;
            this.trackConfigJsonParser = trackConfigJsonParser;
        }

        public (Result Result, TrackConfigObjectTree TrackConfigObjectTree, List<Error> Errors)
            ReadExercises()
        {
            try
            {
                var exerciseJson = trackConfigFileHandler.ReadFile();
                return trackConfigJsonParser.FromString(exerciseJson);
            }
            catch (Exception e)
            {
                return (
                    Result.FatalError,
                    new TrackConfigObjectTree(),
                    new List<Error>
                        {new Error(ErrorSource.Process, Severity.Fatal, "reading exercise.json file: " + e.Message)}
                );
            }
        }
    }
}