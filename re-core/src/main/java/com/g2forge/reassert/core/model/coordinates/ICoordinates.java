package com.g2forge.reassert.core.model.coordinates;

import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.core.api.system.IHasSystem;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.IVertex;

public interface ICoordinates extends IHasSystem, IVertex {
	@SuppressWarnings("hiding")
	public interface ICoordinatesBuilder<Coordinates extends ICoordinates> extends IBuilder<Coordinates> {
		public ICoordinatesBuilder<Coordinates> system(ISystem<?> system);
	}

	@Override
	public default boolean isMaterial() {
		return false;
	}
}
