#include <iostream>
#include<bits/stdc++.h>
// #include <ctype.h>
#include <fstream>
#include <sstream>
using namespace std;


map <char,int> dict1;
map <int,char> dict2;
//  probabilies for each letter in english language
float letter_probs[26] ={0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
0.00978, 0.02360, 0.00150, 0.01974, 0.00074 };

// An array that counts how many time a letter K is found for each for the Nth iteration [N][K]
float letter_times[26][26] = {0}; 
float entropies [26] = {0};
 
// Function to create map to lookup
void create_dict()
{
    for(int i = 0; i < 26; i++)
        dict1[char(65 + i)] = i;    
    for(int i = 26; i < 52; i++)
        dict1[char(71 + i)] = i;    
     
    for(int i = 0; i < 26; i++)
        dict2[i] = char(65 + i);
    for(int i = 26; i < 52; i++)
        dict2[i] = char(71 + i);
         
    return;
}

float entropy (float doc_freq[26], float lang_freq[26]){
    float sum = 0;
    for (int i = 0; i < 26; i++)    {   
        sum = sum + (doc_freq[i] * log(lang_freq[i]));
    }
    return -sum;     
}

string readFileIntoString(const string& path) {
    ifstream input_file(path);
    if (!input_file.is_open()) {
        cerr << "Could not open the file - '"
             << path << "'" << endl;
        exit(EXIT_FAILURE);
    }
    return string((std::istreambuf_iterator<char>(input_file)), std::istreambuf_iterator<char>());
}

// Function to decrypt the string
// according to the shift provided
string decrypt(string message, int shift)
{
    string decipher = "";
    int letter_count = 0;
    for(unsigned long int i = 0; i < message.size(); i++){   
        if(isalpha(message[i]) ){
            int num;
            // looks up the map and
            // subtracts the shift to the index
            if ((dict1[message[i]] - shift) <0) {
                num = (dict1[message[i]] - shift + 26) % 26;
            }
            else if (islower(message[i]) && (dict1[message[i]] - shift <26) ){
                num = (dict1[message[i]] - shift + 26);
            }
            else {
                num = (dict1[message[i]] - shift);
            }

            
            // looks up the second map for the
            // shifted alphabets and adds them
            decipher += dict2[num];

            //add how many times the letter is found
            letter_times[shift][num%26] = letter_times[shift][num%26] + 1;
            letter_count++;
        }

        else {
            decipher += message[i];
        }
    }
    for (int i = 0; i < 25; i++){
        // letter times
        if (letter_times[shift][i] != 0){
            letter_times[shift][i] =letter_times[shift][i]/letter_count;
        }
    }
    return decipher;
}
 
int main( int argc, char** argv){
    string message ="";
    message = readFileIntoString(argv[1]);
    // cout << message << endl;
    create_dict();
    // string possible_words[26]= {""};
    string possible_words[26];

    // for (int n = 0; n <= 25; n++)
    for (int n = 0; n <= 25; n++)    {
        possible_words[n] = decrypt(message, n);
        entropies[n] = entropy(letter_times[n], letter_probs);
        // cout <<  possible_words[n] << endl;
    }

    float min = 10000;
    int minIndex = 0;
        // find min entropy from the entropies array
    for (int i = 0; i < 26; i++)  {
        if (entropies[i] < min) {
            min = entropies[i];
            minIndex = i;
        }
        }
    cout <<  possible_words[minIndex] << endl;
   
     
    return 0;
}
