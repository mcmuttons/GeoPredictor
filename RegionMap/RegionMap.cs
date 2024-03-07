using System;
using System.Net;
using System.Text.Json;

namespace EliteDangerousRegionMap
{
    public class SystemData
    {
        public string Name { get; set; }
        public long ID64 { get; set; }
        public double? X { get; set; }
        public double? Y { get; set; }
        public double? Z { get; set; }
        public GalacticRegion Region { get; set; }
        public SystemBoxel Boxel { get; set; }
    }

    public class GalacticRegion
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

    public class SystemBoxel
    {
        public double X { get; set; }
        public double Y { get; set; }
        public double Z { get; set; }
        public GalacticRegion Region { get; set; }
    }

    public static partial class RegionMap
    {
        private class EDSMSystem
        {
            public string name { get; set; }
            public long id64 { get; set; }
            public EDSMSystemCoords coords { get; set; }
        }

        private class EDSMSystemCoords
        {
            public double x { get; set; }
            public double y { get; set; }
            public double z { get; set; }
        }

        private const double x0 = -49985;
        private const double y0 = -40985;
        private const double z0 = -24105;

        public static GalacticRegion FindRegion(double x, double y, double z)
        {
            var px = (int)((x - x0) * 83 / 4096);
            var pz = (int)((z - z0) * 83 / 4096);

            if (px < 0 || pz < 0 || pz >= RegionMapLines.Length)
            {
                return default;
            }
            else
            {
                var row = RegionMapLines[pz];
                var rx = 0;
                var pv = 0;

                foreach (var (rl, rv) in row)
                {
                    if (px < rx + rl)
                    {
                        pv = rv;
                        break;
                    }
                    else
                    {
                        rx += rl;
                    }
                }

                if (pv == 0)
                {
                    return null;
                }
                else
                {
                    return new GalacticRegion { Id = pv, Name = RegionNames[pv] };
                }
            }
        }

        public static SystemBoxel FindRegionForBoxel(long id64)
        {
            int masscode = (int)(id64 & 7);
            double x = (((id64 >> (30 - masscode * 2)) & (0x3FFF >> masscode)) << masscode) * 10 + x0;
            double y = (((id64 >> (17 - masscode)) & (0x1FFF >> masscode)) << masscode) * 10 + y0;
            double z = (((id64 >> 3) & (0x3FFF >> masscode)) << masscode) * 10 + z0;

            return new SystemBoxel
            {
                X = x,
                Y = y,
                Z = z,
                Region = FindRegion(x, y, z)
            };
        }
    }
}
