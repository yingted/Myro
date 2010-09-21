using System;
using System.Collections;
using System.Collections.Generic;

public static class Conx {

  static int seed = 0; 
  static Random random;
	
  public static void Randomize(int seed) {
	Conx.seed = seed;
	Conx.random = new Random(Conx.seed);
  }
	
  public class Network {
	public List<Layer> layers = new List<Layer>();
	public Dictionary<string,Layer> layer = new Dictionary<string,Layer>();
	public List<Connection> connections = new List<Connection>();
	public int verbosity;

	public Network(int verbosity = 0, int seed = 0) {
	  setSeed(seed);
	  this.verbosity = verbosity;
	  // self.setSeed(x)
      //   self.complete = 0
      //   self.name = name
      //   self.inputMap = []
      //   self.targetMap = []
      //   self.association = []
      //   self.inputs = []
      //   self.targets = []
      //   self.orderedInputs = 0
      //   self.loadOrder = []
      //   self.learning = 1
      //   self.momentum = 0.9
      //   self.resetEpoch = 5000
      //   self.resetCount = 1
      //   self.resetLimit = 1
      //   self.batch = 0
      //   self.epoch = 0
      //   self.totalEpoch = 0
      //   self.count = 0 # number of times propagate is called
      //   self.stopPercent = 1.0
      //   self.sigmoid_prime_offset = 0.1
      //   self.tolerance = 0.4
      //   self.interactive = 0
      //   self.epsilon = 0.1
      //   self.reportRate = 25
      //   self.sweepReportRate = None #1000
      //   self.crossValidationCorpus = ()
      //   self.crossValidationReportLayers = []
      //   self.crossValidationSampleRate = 0
      //   self.crossValidationSampleFile = "sample.cv"
      //   self.patterns = {}
      //   self.patterned = 0 # used for file IO with inputs and targets
      //   self.sharedWeights = 0
      //   self.useCrossValidationToStop = 0
      //   self.saveResults = 0 # will save error, correct, total in sweep()
      //   self.results = []
      //   self.autoCrossValidation = 0
      //   self.autoSaveWeightsFile = None
      //   self.autoSaveWeightsFileFormat = "conx"
      //   self.lastAutoSaveWeightsFilename = None
      //   self.autoSaveNetworkFile = None
      //   self.autoSaveNetworkFileFormat = "conx"
      //   self.lastAutoSaveNetworkFilename = None
      //   self.lastLowestTSSError = sys.maxint # some maximum value (not all pythons have Infinity)
      //   self._cv = False # set true when in cross validation
      //   self._sweeping = 0 # flag set when sweeping through corpus (as apposed to just stepping)
      //   self._maxRandom = 0.1
      //   self.currentSweepCount = None
      //   self.log = None # a pointer to a file-like object, like a Log object
      //   self.echo = False   # if going to a log file, echo it too, if true
      //   self.hyperbolicError = 0 # exaggerate error?
      //   # Quickprop settings:
      //   self._quickprop = 0
      //   self.mu = 1.75 # maximum growth factor
      //   self.splitEpsilon = 0
      //   self.decay = 0.0000
      //   self.cacheConnections = []
      //   self.cacheLayers = []
	  setup();
	}

	public void setup() {
	  // for overloads
	}

	public void addLayers(params int [] layer_sizes){
	  List<Layer> hiddens = new List<Layer>();
	  int hidden_count = 1;
	  Layer temp;
	  for (int i = 0; i < layer_sizes.Length; i++) {
		if (i == 0)
		  temp = layer["input"] = new Layer("input", layer_sizes[i]);
		else if (i == layer_sizes.Length - 1)
		  temp = layer["output"] = new Layer("output", layer_sizes[i]);
		else {
		  if (layer_sizes.Length > 3) {
			string name = String.Format("hidden{0}", hidden_count);
			temp = layer[name] = new Layer(name, layer_sizes[i]);
			hidden_count += 1;
		  } else {
			temp = layer["hidden"] = new Layer("hidden", layer_sizes[i]);
		  }
		  hiddens.Add(temp);
		}
		layers.Add(temp);
	  }
	  string lastName = "input";
	  foreach (Layer hidden in hiddens) {
		connect(lastName, hidden.name);
		lastName = hidden.name;
	  }
	  connect(lastName, "output");
	}

	public void connect(string fromLayerName, string toLayerName) {
	  connections.Add( new Connection(layer[fromLayerName], layer[toLayerName]));
	}

    public void setSeed(int seed=0) {
	  Conx.Randomize(seed);
	  Console.WriteLine("Conx using seed: {0}", Conx.seed);
    }
  }

  public static double [] zeros(int size) {
	return new double [size];
  }

  public static double [,] zeros(int fromSize, int toSize) {
	return new double [fromSize, toSize];
  }

  public static double [] randomArray(int size, double maxRandom) {
	double [] temp = new double [size];
	for (int i = 0; i < temp.Length; i++) {
	  temp[i] = random.Next() * (maxRandom * 2.0) - maxRandom;
	}
	return temp;
  }

  public static double [,] randomArray(int fromSize, int toSize, double maxRandom) {
	double [,] temp = new double [fromSize, toSize];
	for (int i = 0; i < fromSize; i++) {
	  for (int j = 0; j < toSize; j++) {
		temp[i,j] = random.Next() * (maxRandom * 2.0) - maxRandom;
	  }
	}
	return temp;
  }
  
  public class Layer {
	public string name;
	public double [] weight;
	public double [] dweight;
	public double [] delta;
	public double [] wed;
	public double [] wedLast;
	public double [] target;
	public double [] error;
	public double [] activation;
	public double [] netinput;
	public bool targetSet;
	public bool activationSet;
	public bool frozen;
	public int size;
	public double maxRandom;
	
	public Layer(string name, int size, double maxRandom = 0.1) {
	  this.name = name;
	  this.size = size;
	  this.maxRandom = maxRandom;
	  initialize();
	}
	
	public void initialize() {
	  /*
		Initializes important node values to zero for each node in the
		layer (target, error, activation, dbias, delta, netinput, bed).
	  */
	  randomize();
	  dweight = zeros(size);
	  delta = zeros(size);
	  wed = zeros(size);
	  wedLast = zeros(size);
	  target = zeros(size);
	  error = zeros(size);
	  activation = zeros(size);
	  netinput = zeros(size);
	  targetSet = false;
	  activationSet = false;
	  frozen = false;
	}
	  
	public void randomize(bool force = false) {
	  /*
		Initialize node biases to random values in the range [-max, max].
	  */
	  if (force | !frozen) {
		weight = randomArray(size, maxRandom);
	  }
	}
  }
  
  public class Node {
  }

  public class Connection {
	public Layer fromLayer;
	public Layer toLayer;
	public bool active;
	public bool frozen;
	public double [,] weight;
	public double [,] dweight;
	public double [,] wed;
	public double [,] wedLast;

	public Connection(Layer fromLayer, Layer toLayer) {
	  active = true;
	  frozen = false;
	  this.fromLayer = fromLayer;
	  this.toLayer = toLayer;
	  initialize();
	}

	public void initialize() {
	  randomize();
	  dweight = zeros(fromLayer.size, toLayer.size);
	  wed = zeros(fromLayer.size, toLayer.size);
	  wedLast = zeros(fromLayer.size, toLayer.size);
	}

	public void randomize(bool force = false) {
	  if (force | !frozen) {
		weight = randomArray(fromLayer.size, toLayer.size, toLayer.maxRandom);
	  }
	}
	
  }

  public static void Main() {
	Network net = new Network();
	net.addLayers(2, 2, 1);
	Console.WriteLine("Network is: {0}", net);
	Console.WriteLine("Network.layers are:");
	foreach (string name in net.layer.Keys) {
	  Console.WriteLine("layer '{0}':", name);
	  Console.WriteLine("   net.layer[{0}] = {1}", name, net.layer[name].weight);
	}
  }

}