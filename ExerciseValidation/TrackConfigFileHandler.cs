using System.IO;
using ExerciseReport;

namespace ExerciseValidation
{
    public interface ITrackConfigFileHandler
    {
        string ReadFile();
    }

    internal class TrackConfigFileHandler : ITrackConfigFileHandler
    {
        private readonly string trackConfigPathAndFileName;

        public TrackConfigFileHandler(string root, string track)
        {
            trackConfigPathAndFileName = Path.Combine(
                root,
                PathNames.Default.Languages,
                track,
                Constants.TrackConfigFileName);
        }

        public string ReadFile()
        {
            return File.ReadAllText(trackConfigPathAndFileName);
        }

        public void WriteFile(string exerciseJson)
        {
            File.WriteAllText(trackConfigPathAndFileName, exerciseJson);
        }

    }
}