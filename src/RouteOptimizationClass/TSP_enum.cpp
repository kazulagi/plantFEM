#include <iostream>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

class Points
{
	public:
	int Node;
	vector<double> x;
	vector<double> y;
	vector<vector<double> > distance;
	double min=pow(10,10);//最小距離、大きな値を入れておく
	vector<int> route;//正しいルートを格納する箱

	void input();
	void calc_distance();
	void show_distance();
	void show_coordinates();
	void optimal_route_output();
};

void Points::calc_distance()//i=jの場合は0
{
	for(int i=0; i<Node; i++)
	{
		for(int j=0; j<Node; j++)
		{
			if(i!=j)
			{
				distance[i][j]=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
			}
			else
			{
				distance[i][j]=0.;
			}
		}
	}
}

void Points::show_distance()
{
	for(int i=0; i<Node; i++)
	{
		for(int j=0; j<Node; j++)
		{
			cout<<i<<" "<<j<<" "<<distance[i][j]<<endl;
		}
	}
}

void Points::show_coordinates()
{
	for(int i=0; i<Node; i++)
	{
		cout<<i<<" "<<x[i]<<" "<<y[i]<<endl;
	}
}

void Points::input()
{
	//点数読み込み//
	ifstream input;
	input.open("points.txt");
	input>>Node;
	x.resize(Node);
	y.resize(Node);
	distance.resize(Node, vector<double>(Node));
	route.resize(Node);
	
	//座標読み込み//
	for(int i=0; i<Node; i++)
	{
		input>>x[i]>>y[i];
	}
	input.close();
}

void Points::optimal_route_output()
{
	for(int i=0; i<Node; i++) cout<<route[i]<<" ";
	cout<<min<<endl;

	ofstream output;
	output.open("solution.txt");
	for(int i=0; i<Node; i++)
	{

		output<<  x[route[i]]<<" "<<y[route[i]] <<" "<< route[i] << endl;
	}
}

inline void enumerate(Points &points)
{
	
	int nn;
	int n;
	vector<int> order(points.Node);
	double dist;
	for(int i=0; i<points.Node; i++)
	{
		order[i]=i;
	}
	//cout<<"calc_distance :: enumerate :: initialization finished."<<endl;

	//next_permutationによる列挙//
	nn=0;
	do{
		dist=0.;
		for(int i=0; i<points.Node-1; i++)
		{
			dist+=points.distance[order[i]][order[i+1]];
			//cout<<i<<" : next_pernumation"<<endl;
		}
		dist+=points.distance[order[points.Node-1]][order[0]];

		//列挙結果全部出力するとき//
		//for(int i=0; i<points.Node; i++) cout<<order[i]<<" ";
		//cout<<dist<<endl;
		nn=nn+1;
		//cout<<"next_pernumation itr :: "<<nn<<endl;
		if(dist<points.min)
		{
			points.min=dist;
			for(int i=0; i<points.Node; i++) points.route[i]=order[i];
			//cout<<dist<<endl;
		}

		/*if(nn=1000)
		{
			return;
		}*/

	}while(next_permutation(order.begin(),order.end()));
}



int main()
{
	Points points;//点群オブジェクトを生成
	points.input();//点数と座標を読み込む
	points.calc_distance();//任意の二点間距離を計算（距離の対称性を無視）
	cout<<"calc_distance :: finished "<<endl;
	enumerate(points);//全部列挙する（円順列であることを無視）
	points.optimal_route_output();//答えを出力

	return 0;
}