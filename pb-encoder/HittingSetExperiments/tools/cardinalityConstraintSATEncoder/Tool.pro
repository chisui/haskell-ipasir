
QMAKE_CFLAGS_RELEASE += -g
QMAKE_CXXFLAGS_RELEASE += -g -std=gnu++0x
QMAKE_CFLAGS_DEBUG += -g -Wall -Wextra 
QMAKE_CXXFLAGS_DEBUG += -g -std=gnu++0x -Wall -Wextra 

TEMPLATE = app \
    console
CONFIG += release
CONFIG -= app_bundle
CONFIG -= qt
HEADERS += 

SOURCES += main.cpp

TARGET = encoder
INCLUDEPATH = ../../lib/pblib

LIBS += -static -L../../lib/pblib -lpblib

PKGCONFIG += 
QT -= gui \
    core
