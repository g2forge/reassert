package com.g2forge.reassert.core.api.system;

import com.g2forge.reassert.core.model.coordinates.ICoordinates;

@SuppressWarnings("hiding")
public abstract class ARepository<Coordinates extends ICoordinates, System extends ISystem<Coordinates>> implements IRepository<Coordinates> {
	protected void assertValid(Coordinates coordinates) {
		if (!getSystem().isValid(coordinates)) throw new IllegalArgumentException(String.format("Coordinates \"%1$s\" are not valid under the \"%2$s\" system!", coordinates, getSystem()));
	}

	public abstract System getSystem();
}
