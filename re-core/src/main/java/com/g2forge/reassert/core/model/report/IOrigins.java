package com.g2forge.reassert.core.model.report;

import java.util.Collection;

import com.g2forge.reassert.core.model.artifact.Artifact;

public interface IOrigins {
	public Collection<? extends Artifact<?>> getOrigins();
}
