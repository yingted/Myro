using System;
using System.Collections;
using System.Collections.Generic;

public static class Conx {

  static int seed = 0; 
  static Random random;
	
  public static void randomize(int seed) {
	Conx.seed = seed;
	Conx.random = new Random(Conx.seed);
  }
	
  public class Network {
	public Dictionary<string,Layer> layer = new Dictionary<string,Layer>();

	public Network(params int [] layer_sizes){
	  setSeed();
	  int i = 0;
	  int hidden_count = 1;
	  foreach (int size in layer_sizes) {
		if (i == 0)
		  layer["input"] = new Layer("input", size);
		else if (i == layer_sizes.Length - 1)
		  layer["output"] = new Layer("hidden", size);
		else {
		  if (layer_sizes.Length > 3) {
			string name = String.Format("hidden{0}", hidden_count);
			layer[name] = new Layer(name, size);
			hidden_count += 1;
		  } else {
			layer["hidden"] = new Layer("hidden", size);
		  }
		}
	  }
	}

    public void setSeed(int seed=0) {
	  Conx.randomize(seed);
	  Console.WriteLine("Conx using seed: {0}", Conx.seed);
    }
  }

  public static double [] zeros(int size) {
	return new double [size];
  }

  public static double [] randomArray(int size, double maxRandom) {
	double [] temp = new double [size];
	for (int i = 0; i < temp.Length; i++) {
	  temp[i] = random.Next();
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

  public static void Main() {
	Network net = new Network(2, 2, 1);
	Console.WriteLine("Network is: {0}", net);
	Console.WriteLine("Network.layers are:");
	foreach (string name in net.layer.Keys) {
	  Console.WriteLine("layer '{0}':", name);
	  Console.WriteLine("   net.layer[{0}] = {1}", name, net.layer[name].weight);
	}
  }

}