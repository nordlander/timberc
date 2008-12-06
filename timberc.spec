# RPM spec file for timberc				         -*-rpm-spec-*-
#
# This file is subject to the same free software license as timberc.
#
# Copyright 2008, Peter A. Jonsson
#
# This file was derived from ghc.spec.in

%define name    timberc
%define version 1.0.1
%define release 1

Name:           %{name}
Version:        %{version}
Release:        %{release}
License:        BSD-like
Group:          Development/Languages/Timber
URL:            http://timber-lang.org
Source0:        http://timber-lang.org/dist/timberc-%{version}.tar.gz
Packager:       Peter A. Jonsson <pj@csee.ltu.se>
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildRequires:  happy >= 1.18, ghc >= 6.10.1 
Provides:       timber
Summary:        The Timber Compiler

%description

%prep
%setup -b0

%build

runhaskell Setup configure --prefix=%{_prefix}
runhaskell Setup build

%install

runhaskell Setup copy --destdir=%{_tmppath}/%{name}-%{version}-build
mv %{_tmppath}/%{name}-%{version}-build/%{_bindir}/timberc %{_tmppath}/%{name}-%{version}-build/%{_datadir}/timberc-%{version}/timberc


# Ugly hack: construct a temporary bin/timberc by hand to build the RTS.
SCRIPT="%{_bindir}/timberc"
SCRIPT_TMP="%{_tmppath}/%{name}-%{version}-build/$SCRIPT"
DATADIR="%{_datadir}/timberc-%{version}"
DATADIR_TMP="%{_tmppath}/%{name}-%{version}-build/$DATADIR"

echo "#!/bin/sh" > $SCRIPT_TMP
echo " " >> $SCRIPT_TMP
echo "exec $DATADIR_TMP/timberc \${1+\"\$@\"} --datadir $DATADIR_TMP" >> $SCRIPT_TMP
chmod 755 $SCRIPT_TMP

cd rtsPOSIX
sh ./configure --prefix=$DATADIR --with-timberc=$SCRIPT_TMP
make DESTDIR=%{_tmppath}/%{name}-%{version}-build install
cd ..

# We're done with the temporary thing. Now make a real one.
rm $SCRIPT_TMP
echo "#!/bin/sh" > $SCRIPT_TMP
echo " " >> $SCRIPT_TMP
echo "exec $DATADIR/timberc \${1+\"\$@\"} --datadir $DATADIR" >> $SCRIPT_TMP
chmod 755 $SCRIPT_TMP

%clean
rm -rf ${RPM_BUILD_ROOT}

%post
# We should perhaps register as a Haskell package.

%preun
# If we register in post we should unregister here.

%files 
%defattr(-,root,root)
%doc %{_prefix}/share/doc/timberc-%{version}/*
%{_prefix}/bin/timberc
%{_prefix}/share/timberc-%{version}/timberc
%{_prefix}/share/timberc-%{version}/rtsPOSIX/*
%{_prefix}/share/timberc-%{version}/examples/*
%{_prefix}/share/timberc-%{version}/include/*
%{_prefix}/share/timberc-%{version}/lib/*

