package com.g2forge.reassert.core.api.module;

import java.util.Collection;

import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.reassert.cache.ICache;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

public interface IContext {
	public default <S extends ISystem<?>> S findSystem(Class<S> klass) {
		return HStream.findOne(getSystems().stream().filter(klass::isInstance).map(klass::cast));
	}

	public default <Coordinates extends ICoordinates> ISystem<Coordinates> findSystem(Coordinates coordinates) {
		try {
			@SuppressWarnings("unchecked")
			final ISystem<Coordinates> retVal = (ISystem<Coordinates>) HStream.findOne(getSystems().stream().filter(s -> s.isValid(coordinates)));
			return retVal;
		} catch (IllegalArgumentException exception) {
			throw new IllegalArgumentException(String.format("Failed to find system for coordinates: %1$s", coordinates), exception);
		}
	}

	public ICache getCache();

	public Collection<IDescriber<?>> getDescribers();

	public ILicenseParser getLicenseParser();

	public IScanner getScanner();
	
	public Collection<ISystem<?>> getSystems();
}
