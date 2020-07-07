package com.g2forge.reassert.core.api.system;

import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

public interface ISystem<Coordinates extends ICoordinates> {
	public IDescriber<Coordinates> getCoordinateDescriber();

	public default ITypeRef<Coordinates> getCoordinateType() {
		return getCoordinateDescriber().getType();
	}

	public IRepository<Coordinates> getRepository();

	public IScanner getScanner();

	public default boolean isValid(ICoordinates coordinates) {
		return getCoordinateType().isInstance(coordinates) && ((coordinates.getSystem() == this) || (coordinates.getSystem() == null));
	}

	public Coordinates withSystem(Coordinates coordinates);
}
