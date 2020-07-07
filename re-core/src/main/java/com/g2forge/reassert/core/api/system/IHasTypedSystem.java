package com.g2forge.reassert.core.api.system;

import com.g2forge.reassert.core.model.coordinates.ICoordinates;

public interface IHasTypedSystem<Coordinates extends ICoordinates> extends IHasSystem {
	@Override
	public ISystem<Coordinates> getSystem();
}
