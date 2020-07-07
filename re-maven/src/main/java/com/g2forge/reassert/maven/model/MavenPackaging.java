package com.g2forge.reassert.maven.model;

public enum MavenPackaging {
	POM,
	JAR,
	MAVEN_PLUGIN {
		@Override
		public String toString() {
			return "MAVEN-PLUGIN";
		}
	},
	EJB,
	WAR,
	EAR,
	RAR,
	BUNDLE;
}
