#include "pb2cnf.h"
#include <sstream>

int main(int argc, const char **args) {
    if (argc!=4) {
        std::cerr << "Error: Expected exactly three parameters: Encoding selection, number of objects, and how many objects are to be selected at most!\n";
        return 1;
    }
    std::istringstream nofObjectsStream(args[2]);
    std::istringstream howManyStream(args[3]);
    std::string mode = args[1];
    int nofObjects;
    int howMany;
    nofObjectsStream >> nofObjects;
    howManyStream >> howMany;
    if ((nofObjectsStream.fail()) || (nofObjects<=0)) {
        std::cerr << "Error reading number of objects!\n";
        return 1;
    }
    if ((howManyStream.fail()) || (howMany<=0) || (howMany > nofObjects)) {
        std::cerr << "Error reading how many objects are to be selected!\n";
        return 1;
    }

    PBConfig config = std::make_shared<PBConfigClass>();
    if (mode=="BDD") {
        config->amk_encoder = AMK_ENCODER::BDD;
    } else if (mode=="CARDNET") {
        config->amk_encoder = AMK_ENCODER::CARD;
    } else {
        std::cerr << "Illegal encoding type!\n";
        return 1;
    }

    std::vector<int32_t> literals;
    for (int i=1;i<=nofObjects;i++) {
        literals.push_back(i);
    }
    std::vector<std::vector<int32_t> > clauses;
    int firstFreeVariable = nofObjects+1;

    PB2CNF translator(config);
    translator.encodeAtMostK(literals,howMany,clauses,firstFreeVariable);
    for (auto &it : clauses) {
        for (auto it2 : it) {
            std::cout << it2 << " ";
        }
        std::cout << "0\n";
    }
    return 0;
}
