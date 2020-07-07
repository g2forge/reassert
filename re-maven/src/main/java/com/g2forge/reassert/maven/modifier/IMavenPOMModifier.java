package com.g2forge.reassert.maven.modifier;

import java.nio.file.Path;

import com.g2forge.alexandria.java.function.IConsumer2;

public interface IMavenPOMModifier extends IConsumer2<Path, Path> {
	@Override
	public void accept(Path input, Path output);

	public String getKey();
}
