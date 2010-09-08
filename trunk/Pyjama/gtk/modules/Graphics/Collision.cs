using PolygonIntersection;

public class Collision {
    // Check if polygon A is colliding with polygon B
    public static bool PolygonCollision(Polygon polygonA, Polygon polygonB) {
	bool result = true;
	
	int edgeCountA = polygonA.Edges.Count;
	int edgeCountB = polygonB.Edges.Count;
	Vector edge;
	
	// Loop through all the edges of both polygons
	for (int edgeIndex = 0; edgeIndex < edgeCountA + edgeCountB; edgeIndex++) {
	    if (edgeIndex < edgeCountA) {
		edge = polygonA.Edges[edgeIndex];
	    } else {
		edge = polygonB.Edges[edgeIndex - edgeCountA];
	    }
	    
	    // Find the axis perpendicular to the current edge
	    Vector axis = new Vector(-edge.Y, edge.X);
	    axis.Normalize();
	    
	    // Find the projection of the polygon on the current axis
	    float minA = 0; float minB = 0; float maxA = 0; float maxB = 0;
	    ProjectPolygon(axis, polygonA, ref minA, ref maxA);
	    ProjectPolygon(axis, polygonB, ref minB, ref maxB);
	    
	    // Check if the polygon projections are currentlty intersecting
	    if (IntervalDistance(minA, maxA, minB, maxB) > 0) {
		result = false;
		break;
	    }
	}
	return result;
    }
    
    // Calculate the distance between [minA, maxA] and [minB, maxB]
    // The distance will be negative if the intervals overlap
    public static float IntervalDistance(float minA, float maxA, float minB, float maxB) {
	if (minA < minB) {
	    return minB - maxA;
	} else {
	    return minA - maxB;
	}
    }
    
    // Calculate the projection of a polygon on an axis and returns it as a [min, max] interval
    public static void ProjectPolygon(Vector axis, Polygon polygon, ref float min, ref float max) {
	// To project a point on an axis use the dot product
	float d = axis.DotProduct(polygon.Points[0]);
	min = d;
	max = d;
	for (int i = 0; i < polygon.Points.Count; i++) {
	    d = polygon.Points[i].DotProduct(axis);
	    if (d < min) {
		min = d;
	    } else {
		if (d > max) {
		    max = d;
		}
	    }
	}
    }
}
