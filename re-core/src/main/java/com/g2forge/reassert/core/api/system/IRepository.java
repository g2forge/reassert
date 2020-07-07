package com.g2forge.reassert.core.api.system;

import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

public interface IRepository<Coordinates extends ICoordinates> extends IHasTypedSystem<Coordinates> {
	public Artifact<Coordinates> load(Coordinates coordinates, IReassertGraphBuilder builder);
}
