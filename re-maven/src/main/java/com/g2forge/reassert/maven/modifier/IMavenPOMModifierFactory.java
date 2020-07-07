package com.g2forge.reassert.maven.modifier;

import java.util.Collection;

import com.g2forge.reassert.maven.MavenCoordinates;

@FunctionalInterface
public interface IMavenPOMModifierFactory {
	public default Collection<Class<? extends IMavenPOMModifierFactory>> getDownstream() {
		return null;
	}

	public IMavenPOMModifier getModifier(MavenCoordinates coordinates);
}
